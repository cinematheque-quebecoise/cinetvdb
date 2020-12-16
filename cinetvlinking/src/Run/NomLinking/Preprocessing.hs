{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Run.NomLinking.Preprocessing
  ( preprocess
  )
where

import           Import
-- import Run.NomLinking.Model (getPersonFeatures)
import qualified Control.Foldl           as L
import qualified Data.ByteString.Lazy    as BL
import           Data.Csv                (DefaultOrdered, EncodeOptions (..),
                                          ToNamedRecord (..),
                                          defaultEncodeOptions,
                                          encodeDefaultOrderedByNameWith)
import           Data.List               ((!!))
import qualified Data.List               as List
import qualified Data.Set                as Set
import qualified Data.Vector             as Vector
import           Database.CineTV         (MonadGetPersonFeatures (..))
import qualified RIO.Text                as Text
import           System.Directory        (createDirectoryIfMissing,
                                          doesFileExist)
import           System.Random
import           Types.AnnotatedFeatures
import           Types.PersonFeatures

type SampleSize = Integer
type ValidationRatio = Double

nomAnnotatedCsvFile :: FilePath
nomAnnotatedCsvFile = "Nom_LienWikidata.csv"

preprocess :: FilePath -> SampleSize -> ValidationRatio -> RIO App ()
preprocess cinetvExtDir size validationRatio = do

  logInfo "Creating validation and test datasets..."

  let nomAnnotatedCsvFpath = cinetvExtDir <> "/" <> nomAnnotatedCsvFile
  -- annotatedDataE <- readAnnotatedPeopleData "data/nom-wd-annotation.csv"
  annotatedDataE <- readAnnotatedPeopleData nomAnnotatedCsvFpath

  case annotatedDataE of
    Left errMsg -> logError $ display $ Text.pack errMsg
    Right (_, annotatedRecords) -> preprocessRecords size
                                                     validationRatio
                                                     nomAnnotatedCsvFpath
                                                     annotatedRecords

  return ()

-- | Select sample of SampleSize length from streaming records, split into
-- | validation and test set and write results to file
preprocessRecords
  :: (Foldable t)
  => SampleSize
  -> ValidationRatio
  -> FilePath
  -> t (AnnotatedFeatures PersonFeatures)
  -> RIO App ()
preprocessRecords size validationRatio nomAnnotatedCsvFpath annotatedRecords =
  do

    let numAnnotatedInstances = length annotatedRecords
    let numMissingAnnotations = foldr
          (\p acc -> if isAnnotated p then acc else acc + 1 :: Int)
          0
          annotatedRecords

    if size > fromIntegral numAnnotatedInstances
      then do
        logInfo "Not enough annotated instances ... Sampling ..."

        unannotatedData <- getPersonFeatures

        let numToSample = size - fromIntegral numAnnotatedInstances

        let annotatedInstancesIds = foldMap (Set.singleton . personId . features) annotatedRecords

        sample <- sampleUnannotatedRecords numToSample
                                           annotatedInstancesIds
                                           unannotatedData

        annotatedFileExists <- liftIO $ doesFileExist nomAnnotatedCsvFpath

        if annotatedFileExists
          then appendDataSetToFile nomAnnotatedCsvFpath
            $ Vector.map fromUnannotated sample
          else writeDataSetToFile nomAnnotatedCsvFpath
            $ Vector.map fromUnannotated sample
        logInfo
          "Finished sampling unannotated instances. You need to annotated those instances NOW!"
      else if numMissingAnnotations > 0
        then logWarn "Missing annotations. You need to annotated them!"
        else do
          logInfo "Sampling from annotated instances ..."
          sample <- sampleAnnotatedRecords size annotatedRecords
          let (validationSet, testSet) = splitDataset validationRatio sample

          writeDataSetToFile "data/people-annotated-val.csv"  validationSet
          writeDataSetToFile "data/people-annotated-test.csv" testSet

          logInfo "Finished creating validation and test datasets!"

          let idxToSplit = getIndexToSplit size validationRatio

          logInfo ""
          logInfo
            $  "Created "
            <> (display . Text.pack . show) idxToSplit
            <> " instances for validation."

          logInfo
            $  "Created "
            <> (display . Text.pack . show) (size - fromIntegral idxToSplit)
            <> " instances for testing."

    return ()

-- |Select sample of SampleSize elements from streaming records
-- Modify randomizer. The reservoir sampling does not shuffle if size = length tl
sampleAnnotatedRecords :: (Foldable t) => SampleSize -> t a -> RIO App [a]
sampleAnnotatedRecords size records = do
  sample         <- liftIO $ fmap (fromMaybe Vector.empty) foldSample
  liftIO $ shuffle $ Vector.toList sample
  where
    -- foldSample :: IO (Maybe (Vector a))
        foldSample = L.foldM (L.randomN (fromIntegral size)) records

-- |Select sample of SampleSize elements from streaming records
sampleUnannotatedRecords
  :: (Foldable t)
  => SampleSize
  -> Set Int64
  -> t PersonFeatures
  -> RIO App (Vector PersonFeatures)
sampleUnannotatedRecords size annotatedIds records =
  liftIO $ fmap (fromMaybe Vector.empty) foldSample

 where
    -- foldSample :: IO (Maybe (Vector a))
  foldSample = L.foldM
    (L.handlesM (L.filtered isUnknown) (L.randomN (fromIntegral size)))
    records
  isUnknown p = Set.notMember (personId p) annotatedIds

-- |Split dataset to validation and test set
splitDataset :: ValidationRatio -> [a] -> ([a], [a])
splitDataset ratio vec = List.splitAt idxToSplit vec
  where idxToSplit = getIndexToSplit (fromIntegral $ List.length vec) ratio

getIndexToSplit :: SampleSize -> ValidationRatio -> Int
getIndexToSplit size ratio = ceiling $ ratio * fromIntegral size

-- |Write dataset to file
writeDataSetToFile
  :: (Foldable t, DefaultOrdered a, ToNamedRecord a)
  => FilePath
  -> t a
  -> RIO App ()
writeDataSetToFile fpath dataset = do
  liftIO $ createDirectoryIfMissing True "data"

  let myOptions =
        defaultEncodeOptions { encUseCrLf = False, encIncludeHeader = True }
  liftIO
    $ BL.writeFile fpath
    $ encodeDefaultOrderedByNameWith myOptions
    $ toList dataset

appendDataSetToFile
  :: (Foldable t, DefaultOrdered a, ToNamedRecord a)
  => FilePath
  -> t a
  -> RIO App ()
appendDataSetToFile fpath dataset = do
  liftIO $ createDirectoryIfMissing True "data"

  let myOptions =
        defaultEncodeOptions { encUseCrLf = False, encIncludeHeader = False }

  liftIO
    $ BL.appendFile fpath
    $ encodeDefaultOrderedByNameWith myOptions
    $ toList dataset

shuffle :: [a] -> IO [a]
shuffle x = if length x < 2
  then return x
  else do
    i <- randomRIO (0, length x - 1)
    r <- shuffle (take i x ++ drop (i + 1) x)
    return (x !! i : r)
