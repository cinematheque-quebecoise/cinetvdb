{-# LANGUAGE OverloadedStrings #-}

module Database.CQ.Filmo
  ( getFilmoFeaturesIO,
  )
where

import Data.List.Extended (groupByKey)
import Data.Pool (Pool)
import qualified Data.Text as Text
import Database.CineTv.Public.Model
import Database.Esqueleto
import EntityLinking.Filmo.Types (FilmoFeatures (..))
import qualified EntityLinking.Filmo.Types as Filmo
import Import hiding ((^.))

-- TODO: Faire Left Outer Join entre Filmo et Réalisateur
getFilmoFeaturesIO :: (MonadIO m) => Pool SqlBackend -> m [FilmoFeatures]
getFilmoFeaturesIO pool = do
  filmoResults <-
    liftIO $
      flip runSqlPersistMPool pool $
        select $
          distinct $
            from $
              \(filmo, filmoRealisation, nom) -> do
                where_
                  ( (filmo ^. FilmoId ==. filmoRealisation ^. Filmo_RealisationFilmoId)
                      &&. (filmoRealisation ^. Filmo_RealisationNomId ==. nom ^. NomId)
                  )
                return (filmo, nom)

  return $ toFilmosFeatures filmoResults

toFilmosFeatures :: [(Entity Filmo, Entity Nom)] -> [FilmoFeatures]
toFilmosFeatures entities = mapMaybe toFilmoFeatures $ groupByKey (\(f, _) -> entityKey f) entities
  where
    toFilmoFeatures :: [(Entity Filmo, Entity Nom)] -> Maybe FilmoFeatures
    toFilmoFeatures e@((filmoEntity, _) : _) =
      let filmo = entityVal filmoEntity
       in Just $
            FilmoFeatures
              (Filmo.FilmoId $ fromSqlKey $ entityKey filmoEntity)
              (mkTitle filmo)
              (filmoAnneeSortie filmo)
              ( Text.intercalate " ; " $
                  map movieDirectorsToText $
                    groupByKey
                      (\(f, _) -> entityKey f)
                      e
              )
    toFilmoFeatures [] = Nothing

mkTitle :: Filmo -> Text
mkTitle filmo =
  Text.intercalate " " $ catMaybes [filmoPrefixeTitreOriginal filmo, filmoTitreOriginal filmo]

movieDirectorsToText :: [(Entity Filmo, Entity Nom)] -> Text
movieDirectorsToText movieDirectors = maybe "" (mkPersonName . snd) $ listToMaybe movieDirectors

mkPersonName :: Entity Nom -> Text
mkPersonName nomEntity =
  Text.intercalate " " $
    catMaybes
      [ nomPrenom $ entityVal nomEntity,
        nomNom $ entityVal nomEntity
      ]
