{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module EntityLinking.Preprocessing
  ( preprocess,
  )
where

import Data.Csv
  ( DefaultOrdered,
    FromNamedRecord,
    ToNamedRecord (..),
  )
import qualified Data.List as List
import EntityLinking.Dataset (Dataset (Dataset, unDataset), DatasetValue (features), numMissingAnnotations, readDatasetFromCsv, sampleDataset, writeDataset, getVerifiedInstances, fromUnannotated)
import Import
import qualified RIO.Text as Text
import Database.CQ (HasCqEntitiesFeatures (getCqEntitiesFeatures), HasEntityId (entityId))

type SampleSize = Integer

type ValidationRatio = Double

-- | Select sample of SampleSize length from streaming records, split into
-- validation and test set and write results to file
preprocess ::
  forall a.
  ( FromNamedRecord a,
    ToNamedRecord a,
    DefaultOrdered (DatasetValue a),
    HasCqEntitiesFeatures (RIO App) a,
    HasEntityId a
  ) =>
  Proxy a ->
  -- | File containing annotated entity instances
  FilePath ->
  -- | Number of instances to sample
  SampleSize ->
  -- | Ratio for splitting instances to training and validation sets
  ValidationRatio ->
  -- | Base filepath name for output
  FilePath ->
  RIO App ()
preprocess _ annotatedCsvFpath sampleSize validationRatio outputBaseFpath = do
  annotatedDataE <- readDatasetFromCsv @a annotatedCsvFpath
  case annotatedDataE of
    Left errMsg -> logError $ display $ Text.pack errMsg
    Right verifiedDataset -> do
      entitiesFeatures <- getCqEntitiesFeatures @(RIO App) @a

      let verifiedInstances = unDataset $ getVerifiedInstances verifiedDataset
      let verifiedIds = fmap (entityId . features) verifiedInstances
      let unVerifiedDataset = Dataset $ fromUnannotated <$> filter (\p -> entityId p `notElem` verifiedIds) entitiesFeatures

      let numAnnotatedInstances = length (unDataset verifiedDataset)

      logInfo $
        "Dataset contains "
          <> display numAnnotatedInstances
          <> " annotated instances."

      if sampleSize > fromIntegral numAnnotatedInstances
        then do
          logInfo $
            "Not enough annotated instances. Asked for "
              <> display sampleSize
              <> " instances, but "
              <> display numAnnotatedInstances
              <> " are annotated."

          let numToSample = sampleSize - fromIntegral numAnnotatedInstances
          sample <- sampleDataset numToSample unVerifiedDataset
          logInfo $
            "Sampled an additionnal "
              <> display (length sample)
              <> " instances ..."
          writeDataset annotatedCsvFpath sample True

          logInfo $
            "Finished sampling unannotated instances. "
              <> "You need to annotated those instances NOW!"
        else do
          let numMissing = numMissingAnnotations verifiedDataset
          if numMissing > 0
            then
              logWarn $
                display numMissing
                  <> " intances are not annotated. Annotate them before continuing!"
            else do
              logInfo "Sampling from annotated instances ..."
              sample <- sampleDataset sampleSize verifiedDataset
              let (validationSet, testSet) = splitDataset validationRatio (unDataset sample)

              outputDir <- optionsOutputDir . appOptions <$> ask
              writeDataset (outputDir <> "/" <> outputBaseFpath <> "-validationset.csv") (Dataset validationSet) False
              writeDataset (outputDir <> "/" <> outputBaseFpath <> "-testset.csv") (Dataset testSet) False

              logInfo "Finished creating validation and test datasets!"

              let idxToSplit = getIndexToSplit sampleSize validationRatio

              logInfo ""
              logInfo $
                "Created "
                  <> (display . Text.pack . show) idxToSplit
                  <> " instances for validation."

              logInfo $
                "Created "
                  <> (display . Text.pack . show) (sampleSize - fromIntegral idxToSplit)
                  <> " instances for testing."

-- | Split dataset to validation and test set
splitDataset :: ValidationRatio -> [a] -> ([a], [a])
splitDataset ratio vec = List.splitAt idxToSplit vec
  where
    idxToSplit = getIndexToSplit (fromIntegral $ List.length vec) ratio

getIndexToSplit :: SampleSize -> ValidationRatio -> Int
getIndexToSplit size ratio = ceiling $ ratio * fromIntegral size
