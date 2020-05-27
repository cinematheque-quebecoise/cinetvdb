{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Run.NomLinking.Model (GetWikidataPersonsCount(..), GetPersonRoles(..), GetUsersByName(..), getPersonFeatures, getNumResultsForPersonName) where

import Import hiding ((^.))
import qualified RIO.Text as Text
import qualified Data.Text.Read as Text
import Data.Either.Combinators (rightToMaybe)
import qualified RIO.List as List
import Types.PersonFeatures
import Database.CineTv.Model
import Database.Esqueleto
import Data.Pool (Pool)
import Data.List.Extended (groupByKey)

class (Monad m) => GetPersonRoles m where
  getPersonRoles :: Text -> m [Text]

class (Monad m) => GetUsersByName m where
  getUsersByName :: Text -> Text -> m [Text]

class (Monad m) => GetWikidataPersonsCount m where
  getCountsByFullname :: Text -> m Int

instance (HasDbPool env) => GetPersonRoles (RIO env) where
  getPersonRoles personId = do
    pool <- fmap getDbPool ask
    case fmap fst $ rightToMaybe $ Text.decimal personId of
      Just personIdNum -> do
        filmoGenericResults <- liftIO $ flip runSqlPersistMPool pool $ do
          select $
            distinct $
            from $ \(filmo, filmoGenerique, nom, fonction) -> do
            where_ ( nom ^. NomId ==. valkey personIdNum
                 &&. filmo ^. FilmoId ==. filmoGenerique ^. Filmo_GeneriqueFilmoId
                 &&. filmoGenerique ^. Filmo_GeneriqueFonctionId ==. fonction ^. FonctionId
                 &&. filmoGenerique ^. Filmo_GeneriqueNomId ==. just (nom ^. NomId)
                   )
            return (fonction)

        filmoRealisationResults <- liftIO $ flip runSqlPersistMPool pool $ do
          select $
            distinct $
            from $ \(filmo, filmoRealisation, nom) -> do
            where_ ( nom ^. NomId ==. valkey personIdNum
                 &&. filmo ^. FilmoId ==. filmoRealisation ^. Filmo_RealisationFilmoId
                 &&. filmoRealisation ^. Filmo_RealisationNomId ==. nom ^. NomId
                   )
            return (filmo)

        return $ fmap toFunctionId filmoGenericResults
              <> if length filmoRealisationResults > 0 then ["realisation"] else []

      Nothing -> return []

toFunctionId :: (Entity Fonction)
             -> Text
toFunctionId fonctionEntity = Text.pack $ show $ fromSqlKey $ entityKey fonctionEntity

instance (HasDbPool env) => GetUsersByName (RIO env) where
  getUsersByName firstname lastname = do
    pool <- fmap getDbPool ask
    results <- liftIO $ flip runSqlPersistMPool pool $ do
      select $
        distinct $
        from $ \(nom) -> do
        where_ (   ( nom ^. NomPrenom ==. (just $ val firstname)
                 &&. nom ^. NomNom ==. (just $ val lastname)
                   )
               ||. ( nom ^. NomNom ==. (just $ val firstname)
                 &&. nom ^. NomPrenom ==. (just $ val lastname)
                   )
               ||. ( nom ^. NomNom ==. (just $ val $ combinedFirstThenLastname) )
               ||. ( nom ^. NomNom ==. (just $ val combinedLastThenFirstname) )
               ||. ( nom ^. NomPrenom ==. (just $ val $ combinedFirstThenLastname) )
               ||. ( nom ^. NomPrenom ==. (just $ val combinedLastThenFirstname) )
               )
        return (nom)

    return $ fmap (Text.pack . show . fromSqlKey . entityKey) results

    where
      combinedFirstThenLastname = Text.strip $ firstname <> " " <> lastname
      combinedLastThenFirstname = Text.strip $ lastname <> " " <> firstname

-- TODO: Faire Left Outer Join entre Filmo et Pays + Filmo et Réalisateur
getPersonFeatures :: (MonadIO m)
                  => Pool SqlBackend
                  -> m [PersonFeatures]
getPersonFeatures pool = do
  filmoGenericResults <- liftIO $ flip runSqlPersistMPool pool $ do
    select $
      distinct $
      from $ \(filmo, filmoGenerique, nom, fonction) -> do
      where_ ( filmo ^. FilmoId ==. filmoGenerique ^. Filmo_GeneriqueFilmoId
           &&. filmoGenerique ^. Filmo_GeneriqueFonctionId ==. fonction ^. FonctionId
           &&. filmoGenerique ^. Filmo_GeneriqueNomId ==. nom ?. NomId
             )
      return (filmo, nom, fonction)

  let credits = catMaybes $ fmap creditVals filmoGenericResults

  return $ creditsToPersonFeatures credits

creditVals :: (Entity Filmo, Maybe (Entity Nom), Entity Fonction)
           -> Maybe (Entity Filmo, Entity Nom, Entity Fonction)
creditVals (filmo, nomMaybe, fonction) = do
  nom <- nomMaybe
  Just $ (filmo, nom, fonction)

creditsToPersonFeatures :: [(Entity Filmo, Entity Nom, Entity Fonction)]
                       -> [PersonFeatures]
creditsToPersonFeatures credits =
    ( catMaybes
    . map personCreditsToFeatures
    . groupByKey (\(_, nomEntity, _) -> entityKey nomEntity)
    ) credits

personCreditsToFeatures :: [(Entity Filmo, Entity Nom, Entity Fonction)]
                        -> Maybe PersonFeatures
personCreditsToFeatures personCredits =
  fmap toPersonFeatures $ listToMaybe personCredits

  where
    toPersonFeatures :: (Entity Filmo, Entity Nom, Entity Fonction)
                     -> PersonFeatures
    toPersonFeatures (_, nomEntity, _) =
      let nomId = Text.pack $ show $ fromSqlKey $ entityKey nomEntity
          firstname = fromMaybe "" $ nomPrenom $ entityVal nomEntity
          lastname = fromMaybe "" $ nomNom $ entityVal nomEntity
       in PersonFeatures nomId
                         firstname
                         lastname
                         ""
                         ( Text.intercalate " ; "
                         $ map personMovieRoleToText
                         $ groupByKey (\(fe, _, _) -> entityKey fe) personCredits
                         )

personMovieRoleToText :: [(Entity Filmo, Entity Nom, Entity Fonction)]
                      -> Text
personMovieRoleToText movieCredits =
  let movieText = fromMaybe "" $ do
        (filmoEntity, _, _) <- listToMaybe movieCredits
        let movieTitle = fromMaybe "" $ filmoTitreOriginal $ entityVal filmoEntity
        let movieYear = filmoAnneeSortie $ entityVal filmoEntity
        let movieYearText = "(" <> (fromMaybe "" $ fmap (Text.pack . show) $ movieYear) <> ")"
        return $ Text.intercalate " " [movieTitle, movieYearText]
      movieFunctions = Text.intercalate " ; "
                     $ List.nub
                     $ map (\(_, _, fonctionEntity) -> fonctionTerme $ entityVal fonctionEntity) movieCredits
  in Text.intercalate " - " [movieText, movieFunctions]

getNumResultsForPersonName :: (MonadIO m)
                           => Pool SqlBackend
                           -> Text
                           -> Text
                           -> m Int
getNumResultsForPersonName pool firstname lastname = do
  results <- liftIO $ flip runSqlPersistMPool pool $ do
    select $
      distinct $
      from $ \(nom) -> do
      where_ (   ( nom ^. NomPrenom ==. (just $ val firstname)
               &&. nom ^. NomNom ==. (just $ val lastname)
                 )
             ||. ( nom ^. NomNom ==. (just $ val firstname)
               &&. nom ^. NomPrenom ==. (just $ val lastname)
                 )
             ||. ( nom ^. NomNom ==. (just $ val $ combinedFirstThenLastname) )
             ||. ( nom ^. NomNom ==. (just $ val combinedLastThenFirstname) )
             ||. ( nom ^. NomPrenom ==. (just $ val $ combinedFirstThenLastname) )
             ||. ( nom ^. NomPrenom ==. (just $ val combinedLastThenFirstname) )
             )
      return (nom)

  return $ length results

  where
    combinedFirstThenLastname = Text.strip $ firstname <> " " <> lastname
    combinedLastThenFirstname = Text.strip $ lastname <> " " <> firstname
