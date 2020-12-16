{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}
module Database.CineTV
  ( MonadGetPersonRoles(..)
  , PersonName(..)
  , MonadGetPersonName(..)
  , MonadGetPersonFeatures(..)
  , MonadGetPersonLinkedIds(..)
  )
where

import qualified Data.List                    as L
import           Data.List.Extended           (groupByKey)
import           Database.CineTv.Public.Model
import           Import                       hiding ((^.))
import           Types.PersonFeatures

import qualified Data.List.NonEmpty           as L hiding (nub)
import           Data.Pool                    (Pool)
import qualified Data.Text                    as T
import           Database.Esqueleto

data PersonName = PersonName
    { personFirstName :: Maybe Text
    , personLastName  :: Maybe Text
    }
    deriving (Eq, Ord)

instance Show PersonName where
  show (PersonName fn ln) =
    T.unpack $ T.strip $ fromMaybe "" $ fn <> Just " " <> ln

class (Monad m) => MonadGetPersonRoles m where
  getPersonRoles :: Int64 -> m [Int64]

class (Monad m) => MonadGetPersonName m where
  getPersonName :: Int64 -> m (Maybe PersonName)

class (Monad m) => MonadGetPersonFeatures m where
  getPersonFeatures :: m [PersonFeatures]

class (Monad m) => MonadGetPersonLinkedIds m where
  getPersonLinkedIds :: m [Key Nom]

instance (HasDbPool env) => MonadGetPersonRoles (RIO env) where
  getPersonRoles nomId = do
    pool <- fmap getDbPool ask
    getPersonRolesIO pool nomId

getPersonRolesIO :: (MonadIO m) => Pool SqlBackend -> Int64 -> m [Int64]
getPersonRolesIO pool nomId = do
  filmoGenericResults <-
    liftIO
    $ flip runSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \(filmoGenerique, nom, fonction) -> do
        where_
          (   (nom ^. NomId ==. valkey nomId)
          &&. (   filmoGenerique
              ^.  Filmo_GeneriqueFonctionId
              ==. fonction
              ^.  FonctionId
              )
          &&. (filmoGenerique ^. Filmo_GeneriqueNomId ==. just (nom ^. NomId))
          )
        return $ fonction ^. FonctionId

  filmoRealisationResults <-
    liftIO
    $ flip runSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \(filmo, filmoRealisation, nom) -> do
        where_
          (   (nom ^. NomId ==. valkey nomId)
          &&. (   filmo
              ^.  FilmoId
              ==. filmoRealisation
              ^.  Filmo_RealisationFilmoId
              )
          &&. (filmoRealisation ^. Filmo_RealisationNomId ==. nom ^. NomId)
          )
        return filmo

  return
    $  fmap (\(Value n) -> fromSqlKey n) filmoGenericResults
    <> [ 1 | not (null filmoRealisationResults) ]

instance (HasDbPool env) => MonadGetPersonName (RIO env) where
  getPersonName nomId = do
    pool <- fmap getDbPool ask
    getPersonNameIO pool nomId

getPersonNameIO
  :: (MonadIO m) => Pool SqlBackend -> Int64 -> m (Maybe PersonName)
getPersonNameIO pool nomId = do
  nomEntities <-
    liftIO $ flip runSqlPersistMPool pool $ select $ distinct $ from $ \nom ->
      do
        where_ (nom ^. NomId ==. valkey nomId)
        return (nom ^. NomPrenom, nom ^. NomNom)

  return $ (\(Value p, Value n) -> PersonName p n) <$> listToMaybe nomEntities

instance (HasDbPool env) => MonadGetPersonFeatures (RIO env) where
  getPersonFeatures = do
    pool <- fmap getDbPool ask
    getPersonFeaturesIO pool

-- TODO: Faire Left Outer Join entre Filmo et Pays + Filmo et Réalisateur
getPersonFeaturesIO :: (MonadIO m) => Pool SqlBackend -> m [PersonFeatures]
getPersonFeaturesIO pool = do
  filmoGenericResults <-
    liftIO
    $ flip runSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \(filmo, filmoGenerique, nom, fonction) -> do
        where_
          (   (filmo ^. FilmoId ==. filmoGenerique ^. Filmo_GeneriqueFilmoId)
          &&. (   filmoGenerique
              ^.  Filmo_GeneriqueFonctionId
              ==. fonction
              ^.  FonctionId
              )
          &&. (filmoGenerique ^. Filmo_GeneriqueNomId ==. nom ?. NomId)
          )
        return (filmo, nom, fonction)

  let credits = mapMaybe creditVals filmoGenericResults

  return $ creditsToPersonFeatures credits

creditVals
  :: (Entity Filmo, Maybe (Entity Nom), Entity Fonction)
  -> Maybe (Entity Filmo, Entity Nom, Entity Fonction)
creditVals (filmo, nomMaybe, fonction) = do
  nom <- nomMaybe
  Just (filmo, nom, fonction)

creditsToPersonFeatures
  :: [(Entity Filmo, Entity Nom, Entity Fonction)] -> [PersonFeatures]
creditsToPersonFeatures = mapMaybe personCreditsToFeatures
  . groupByKey (\(_, nomEntity, _) -> entityKey nomEntity)

personCreditsToFeatures
  :: [(Entity Filmo, Entity Nom, Entity Fonction)] -> Maybe PersonFeatures
personCreditsToFeatures personCredits = toPersonFeatures
  <$> listToMaybe personCredits

 where
  toPersonFeatures
    :: (Entity Filmo, Entity Nom, Entity Fonction) -> PersonFeatures
  toPersonFeatures (_, nomEntity, _) =
    let nomId = fromSqlKey $ entityKey nomEntity
        fns   = fromMaybe "" $ nomPrenom $ entityVal nomEntity
        lns   = fromMaybe "" $ nomNom $ entityVal nomEntity
        fn =
            T.strip
              $ maybe "" (L.maximumBy (\x y -> compare (T.length x) (T.length y)))
              $ L.nonEmpty (T.splitOn "_x000D_" fns)
        ln =
            T.strip
              $ maybe "" (L.maximumBy (\x y -> compare (T.length x) (T.length y)))
              $ L.nonEmpty (T.splitOn "_x000D_" lns)
    in  PersonFeatures
          nomId
          fn
          ln
          ""
          (T.intercalate " ; " $ map personMovieRoleToText $ groupByKey
            (\(fe, _, _) -> entityKey fe)
            personCredits
          )

personMovieRoleToText :: [(Entity Filmo, Entity Nom, Entity Fonction)] -> Text
personMovieRoleToText movieCredits =
  let movieText = fromMaybe "" $ do
        (filmoEntity, _, _) <- listToMaybe movieCredits
        let movieTitle =
              fromMaybe "" $ filmoTitreOriginal $ entityVal filmoEntity
        let movieYear     = filmoAnneeSortie $ entityVal filmoEntity
        let movieYearText = "(" <> maybe "" (T.pack . show) movieYear <> ")"
        return $ T.intercalate " " [movieTitle, movieYearText]
      movieFunctions = T.intercalate " ; " $ L.nub $ map
        (\(_, _, fonctionEntity) -> fonctionTerme $ entityVal fonctionEntity)
        movieCredits
  in  T.intercalate " - " [movieText, movieFunctions]

instance (HasDbPool env) => MonadGetPersonLinkedIds (RIO env) where
  getPersonLinkedIds = do
    pool <- fmap getDbPool ask
    getPersonLinkedIdsIO pool

getPersonLinkedIdsIO :: (MonadIO m) => Pool SqlBackend -> m [Key Nom]
getPersonLinkedIdsIO pool = do
  nomLienWikidataEntities <-
    liftIO
    $ flip runSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \nomLienWikidata -> return (nomLienWikidata ^. Nom_LienWikidataNomId)
  return $ (\(Value nomId) -> nomId) <$> nomLienWikidataEntities

