{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Run.NomLinking.Apply (applyAlgorithm) where

import Import
import Run.NomLinking.Algorithm (runAlgorithm)
import Types.AlgorithmResult (AlgorithmResult(..), readAlgorithmResults)
import Types.PersonFeatures
import Run.NomLinking.Model (getPersonFeatures)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Set as Set
import Data.Csv (DefaultOrdered(..))
import Data.Csv.Streaming (Records)
import Data.Csv (defaultEncodeOptions, EncodeOptions(..))
import Data.Csv.Incremental (encodeDefaultOrderedByNameWith, encodeNamedRecord)
import qualified Data.Text as Text
import Pipes ((>->), await, Consumer)
import qualified Pipes as Pipes (runEffect, each)
import qualified Pipes.Prelude as Pipes (map, mapM)
import System.FilePath (joinPath)
import System.IO.Error (isDoesNotExistError)
import System.Directory (doesFileExist)

applyAlgorithm :: Bool -> RIO App ()
applyAlgorithm willAnnotateAll = do
  logInfo $ "Linking Nom table to Wikidata..."

  outputdir <- fmap (optionsOutputDir . appOptions) ask

  allPersonFeatures <- fmap getDbPool ask >>= (liftIO . getPersonFeatures)

  let fpath = joinPath [outputdir, "NomWd.csv"]

  -- RIO App (Either String [AlgorithmResult PersonFeatures])
  -- case algoResultsEither of
  --   Left err -> do
  --     logError $ display $ Text.pack err
  --     exitFailure
  e <- tryJust (guard . isDoesNotExistError) $ do
    annotatedPersonFeaturesE <- readAnnotatedPersonFeatures fpath
    case annotatedPersonFeaturesE of
      Left err -> do
        logError $ display $ Text.pack err
        exitFailure
      Right annotatedPersonFeatures -> return annotatedPersonFeatures

  let annotatedData = either (const []) id e
  let dataToAnnotate = getDataToPredict willAnnotateAll allPersonFeatures annotatedData

  logInfo $ display $ (Text.pack $ show $ length annotatedData)
                   <> " instances are already annotated!"
  logInfo $ display $ (Text.pack $ show $ length dataToAnnotate)
                   <> " will be annotated..."

  fpathExists <- liftIO $ doesFileExist fpath
  when (willAnnotateAll || not fpathExists) $ do
    let headerLine = Text.intercalate ","
                   $ fmap (decodeUtf8With (\_ _ -> Nothing))
                   $ toList
                   $ headerOrder (undefined :: AlgorithmResult PersonFeatures)

    writeFileUtf8 fpath $ headerLine <> "\r\n"

  savePredictedOutputOfPersonFeatures fpath dataToAnnotate

readAnnotatedPersonFeatures :: (MonadIO m)
                            => FilePath
                            -> m (Either String [PersonFeatures])
readAnnotatedPersonFeatures fpath = do
  algoResultsCsv <- readAlgorithmResults fpath
  let algoResultsE = fmap (recordsToList . snd) algoResultsCsv
  return $ fmap (fmap input . (foldr (\p acc -> mconcat [[p], acc]) [])) algoResultsE
  -- case algoResultsEither of
  --   Left err -> do
  --     logError $ display $ Text.pack err
  --     exitFailure
  --   Right annotatedDataRecords -> do
  --     return $ fmap input $ foldr (\p acc -> mconcat [[p], acc]) [] annotatedDataRecords

getDataToPredict :: Bool -- ^ If true, will reannotated all data!
                 -> [PersonFeatures] -- ^ Person features from database
                 -> [PersonFeatures] -- ^ Already annotated data
                 -> [PersonFeatures]
getDataToPredict willAnnotateAll allPersonFeatures annotatedData =
  let annotatedDataIdsSet = foldr Set.insert Set.empty $ fmap personId annotatedData
      unannotatedData = filter (\p -> Set.notMember (personId p) annotatedDataIdsSet) allPersonFeatures
   in if willAnnotateAll then annotatedData ++ unannotatedData else unannotatedData

savePredictedOutputOfPersonFeatures :: (HasLogFunc env, HasDbPool env)
                                    => FilePath
                                    -> [PersonFeatures] -- ^ Data to annotate
                                    -> RIO env ()
savePredictedOutputOfPersonFeatures fpath dataToAnnotate = do
  Pipes.runEffect $
        Pipes.each dataToAnnotate
    >-> Pipes.mapM predictPersonFeaturesOutput
    >-> Pipes.map ((encodeDefaultOrderedByNameWith myOptions) . encodeNamedRecord)
    >-> appendFileConsumer fpath

  where
    myOptions = defaultEncodeOptions {
      encIncludeHeader = False
    }

predictPersonFeaturesOutput :: (HasLogFunc env, HasDbPool env)
                            => PersonFeatures
                            -> RIO env (AlgorithmResult PersonFeatures)
predictPersonFeaturesOutput personFeatures = do
  logInfo $ display $ "Linking NomID " <> personId personFeatures <> "..."
  predictedValue <- fmap (fromMaybe "NA") $ runAlgorithm personFeatures
  logInfo $ display $ " to " <> predictedValue
  return $ AlgorithmResult personFeatures predictedValue

appendFileConsumer :: (MonadIO m)
                   => FilePath
                   -> Consumer BL.ByteString m ()
appendFileConsumer fpath = forever $ do
  result <- Pipes.await
  liftIO $ BL.appendFile fpath result

recordsToList :: Records a -> [a]
recordsToList records = foldr (\p acc -> mconcat [[p], acc]) [] records
