{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module EntityLinking.Apply where

import qualified Data.ByteString.Lazy as BL
import Data.Csv (DefaultOrdered (..), EncodeOptions (..), FromNamedRecord, ToNamedRecord, defaultEncodeOptions, encodeByName)
import Data.Csv.Incremental (encodeDefaultOrderedByNameWith, encodeNamedRecord)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Database.CQ (HasCqEntitiesFeatures (getCqEntitiesFeatures), HasEntityId (entityId), MonadGetLinkedEntitiesIds (..))
import EntityLinking.Dataset (Dataset (unDataset), DatasetValue (features), readDatasetFromCsv)
import EntityLinking.Evaluation.Types (AlgorithmResult (AlgorithmResult, input))
import EntityLinking.Types (LinkingException, ResourceUri (..))
import EntityLinking.Wikidata (MonadWikidataLinking (..))
import Import
import System.Directory (doesFileExist)
import System.IO.Error (isDoesNotExistError)

writeDisambiguatedEntities ::
  forall a env.
  ( MonadGetLinkedEntitiesIds (RIO env) a,
    HasCqEntitiesFeatures (RIO env) a,
    FromNamedRecord (AlgorithmResult a),
    HasLogFunc env,
    ToNamedRecord (AlgorithmResult a),
    MonadWikidataLinking (RIO env) a,
    DefaultOrdered (AlgorithmResult a)
  ) =>
  Proxy a ->
  Bool ->
  FilePath ->
  RIO env ()
writeDisambiguatedEntities _ willAnnotateAll outputFpath = do
  manuallyAnnotatedEntitiesKeys <- getLinkedEntitiesIds (Proxy @a)
  let manuallyAnnotatedEntitiesIds = Set.fromList manuallyAnnotatedEntitiesKeys

  allEntitiesFeatures <- getCqEntitiesFeatures @(RIO env) @a

  e <- tryJust (guard . isDoesNotExistError) $ do
    annotatedEntitiesFeaturesE <- readDatasetFromCsv @(AlgorithmResult a) outputFpath
    case annotatedEntitiesFeaturesE of
      Left _ -> do
        return []
      Right annotatedEntitiesFeatures ->
        return $ features <$> unDataset annotatedEntitiesFeatures
  let autoAnnotatedEntities = fromRight [] e

  let filteredAutoAnnotatedEntities = filter (\p -> Set.notMember (entityId $ input p) manuallyAnnotatedEntitiesIds) autoAnnotatedEntities
  let filteredAutoAnnotatedEntitiesIds = Set.fromList $ entityId . input <$> filteredAutoAnnotatedEntities

  -- Remove already annotated data from CSV file
  when (length autoAnnotatedEntities /= length filteredAutoAnnotatedEntities) $ do
    let header = headerOrder (undefined :: AlgorithmResult a)
    liftIO $ BL.writeFile outputFpath $ encodeByName header filteredAutoAnnotatedEntities

  let allAnnotatedEntitiesIds = manuallyAnnotatedEntitiesIds <> filteredAutoAnnotatedEntitiesIds
  let entitiesToAnnotate =
        if willAnnotateAll
          then filter (\p -> Set.notMember (entityId p) manuallyAnnotatedEntitiesIds) allEntitiesFeatures
          else filter (\p -> Set.notMember (entityId p) allAnnotatedEntitiesIds) allEntitiesFeatures

  logInfo $
    display $
      Text.pack (show $ if willAnnotateAll then length manuallyAnnotatedEntitiesIds else length allAnnotatedEntitiesIds)
        <> " instances are already annotated!"
  logInfo $
    display $
      Text.pack (show $ length entitiesToAnnotate)
        <> " will be annotated..."

  fpathExists <- liftIO $ doesFileExist outputFpath
  when (willAnnotateAll || not fpathExists || null autoAnnotatedEntities) $ do
    let headerLine =
          Text.intercalate "," $
            fmap (decodeUtf8With (\_ _ -> Nothing)) $
              toList $
                headerOrder (undefined :: AlgorithmResult a)
    writeFileUtf8 outputFpath $ headerLine <> "\r\n"

  savePredictedOutputOfEntityFeatures outputFpath entitiesToAnnotate

savePredictedOutputOfEntityFeatures ::
  forall a env.
  ( HasLogFunc env,
    MonadWikidataLinking (RIO env) a,
    DefaultOrdered (AlgorithmResult a),
    ToNamedRecord (AlgorithmResult a)
  ) =>
  FilePath ->
  -- | Data to annotate
  [a] ->
  RIO env ()
savePredictedOutputOfEntityFeatures fpath dataToAnnotate =
  withFile fpath AppendMode $ \h ->
    pooledForConcurrently_ dataToAnnotate $ \entity ->
      handle (errorHandler entity) $ do
        entityOutput <- predictEntityOutput (Proxy @a) (entityId entity)
        let algoResult = AlgorithmResult entity (fromRight Nothing $ Just . unResourceUri <$> entityOutput)
        let result = encodeDefaultOrderedByNameWith myOptions $ encodeNamedRecord algoResult
        liftIO $ BL.hPut h result
  where
    errorHandler entity (SomeException e) = do
      logWarn $ display $ "Runtime error when linking " <> Text.pack (show $ entityId entity)
      logWarn $ displayShow e

    myOptions = defaultEncodeOptions {encIncludeHeader = False}

predictEntityOutput ::
  forall a env.
  ( HasLogFunc env,
    MonadWikidataLinking (RIO env) a
  ) =>
  Proxy a ->
  Int64 ->
  RIO env (Either LinkingException ResourceUri)
predictEntityOutput p eid = do
  linkedEntityE <- getWikidataLink p eid
  logInfo $
    display $
      "Linking ID "
        <> Text.pack (show eid)
        <> " to "
        <> Text.pack (show $ fromRight "" $ unResourceUri <$> linkedEntityE)
  return linkedEntityE
