{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module EntityLinking.Evaluation
  ( applyAlgorithmToAnnotatedData,
    showEvaluationResults,
  )
where

-- import           Run.NomLinking.Algorithm         (runAlgorithm)

import qualified Data.ByteString.Lazy as BL
import Data.Csv
  ( DefaultOrdered,
    EncodeOptions (..),
    FromNamedRecord,
    Header,
    ToNamedRecord,
    defaultEncodeOptions,
    headerOrder,
  )
import Data.Csv.Incremental
  ( encodeDefaultOrderedByNameWith,
    encodeNamedRecord,
  )
import qualified Data.Map as Map
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MMap
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as Text (toStrict)
import qualified Data.Text.Lazy.Builder as Text
import qualified Data.Text.Lazy.Builder.Int as Text
import qualified Data.Text.Lazy.Builder.RealFloat as Text
import Database.CQ
import EntityLinking.Dataset (Dataset (..), DatasetValue (features), readDatasetFromCsv)
import qualified EntityLinking.Dataset as Dataset
import EntityLinking.Evaluation.Types (AlgorithmResult (..))
import EntityLinking.Nom.Wikidata (linkNomId)
import EntityLinking.Types (LinkingException)
import Import
import Text.Tabl
  ( Alignment (..),
    Decoration (..),
    Environment (EnvAscii),
    tabl,
  )
import EntityLinking.Nom.Types
import EntityLinking.Types (ResourceUri (..))
import EntityLinking.Filmo.Types (FilmoFeatures, FilmoId(..))
import EntityLinking.Filmo.Wikidata (linkFilmoId)
import EntityLinking.Wikidata (MonadWikidataLinking(getWikidataLink))

data EvaluationResults = EvaluationResults
  { -- | Number of true positives
    tp :: Integer,
    -- | Number of false positives
    fp :: Integer,
    -- | Number of false negatives
    fn :: Integer
  }

instance Semigroup EvaluationResults where
  (<>) (EvaluationResults tp1 fp1 fn1) (EvaluationResults tp2 fp2 fn2) =
    EvaluationResults (tp1 + tp2) (fp1 + fp2) (fn1 + fn2)

instance Monoid EvaluationResults where
  mempty = EvaluationResults 0 0 0

applyAlgorithmToAnnotatedData ::
  forall a.
  ( FromNamedRecord a,
    MonadWikidataLinking (RIO App) a,
    ToNamedRecord (AlgorithmResult (DatasetValue a)),
    DefaultOrdered (AlgorithmResult (DatasetValue a))
  ) =>
  Proxy a ->
  FilePath ->
  FilePath ->
  RIO App ()
applyAlgorithmToAnnotatedData _ inputFpath outputFpath = do
  eitherRecords <- readDatasetFromCsv @a inputFpath
  case eitherRecords of
    Left errMsg -> logError $ display $ Text.pack errMsg
    Right dataset -> saveEvaluationResults outputFpath dataset

saveEvaluationResults ::
  forall a env.
  ( MonadWikidataLinking (RIO env) a,
    ToNamedRecord (AlgorithmResult (DatasetValue a)),
    DefaultOrdered (AlgorithmResult (DatasetValue a))
  ) =>
  FilePath ->
  Dataset a ->
  RIO env ()
saveEvaluationResults fpath dataset = do
  writeCSVHeader fpath $ headerOrder (undefined :: (AlgorithmResult (DatasetValue a)))

  withFile fpath AppendMode $ \h -> pooledForConcurrently_ (unDataset dataset) $ \r -> do
    linkedEntityE <- getWikidataLink (Proxy @a) (entityId $ features r)
    let personOutput = case linkedEntityE of
          Right linkedPerson ->
            AlgorithmResult r (Just $ unResourceUri linkedPerson)
          Left _ -> AlgorithmResult r Nothing

    let result =
          encodeDefaultOrderedByNameWith myOptions $
            encodeNamedRecord personOutput
    liftIO $ BL.hPut h result
  where
    myOptions = defaultEncodeOptions {encIncludeHeader = False}

writeCSVHeader :: MonadIO m => FilePath -> Header -> m ()
writeCSVHeader fpath header = do
  let headerLine =
        Text.intercalate "," $ decodeUtf8With (\_ _ -> Nothing) <$> toList header
  writeFileUtf8 fpath $ headerLine <> "\r\n"

{-
=== Summary ===

Correctly classified instances 200 75%
Incorrectly classified instances 50 25%
Total number of instances 234

=== Detailed Accuracy By Class ===

Class | TP | FP | FN | Precision | Recall | F1
NA    | 1 | 1 | 1 | 50% | 50% | 50%
Q*    | 1 | 2 | 2 | 33% | 33% | 33%

Class | NA | Q1334312 | Q4234234 | Q6123554 | Q123456 <--- classified as
NA    | 1  |   0      |    0     |    0     |   1
Q1334312 | 1 |  0     |    0     |    0     |   0
Q4234234 | 0 |  0     |    0     |    1     |   0
Q6123554 | 0 |  0     |    0     |    0     |   0
Q123456 |  0 |  0     |    0     |    0     |   1
 -}
showEvaluationResults ::
  forall a.
  ( FromNamedRecord (AlgorithmResult (DatasetValue a))
  ) =>
  Proxy a ->
  FilePath ->
  RIO App ()
showEvaluationResults _ inputFpath= do
  eitherResults <- readDatasetFromCsv @(AlgorithmResult (DatasetValue a)) inputFpath
  case eitherResults of
    Left errMsg -> logError $ display $ Text.pack errMsg
    Right dataset -> do
      let (correctInstances, totalInstances) = numCorrectlyClassified dataset
      let percentCorrectInstances =
            fromIntegral correctInstances * 100 / fromIntegral totalInstances :: Double
      let percentIncorrectInstances = 100.0 - percentCorrectInstances

      liftIO $ Text.putStrLn "=== Summary ===\n"
      liftIO $
        Text.putStrLn $
          "Correctly classified instances "
            <> intToText correctInstances
            <> "\t"
            <> realFloatToText percentCorrectInstances
            <> "%"
      liftIO $
        Text.putStrLn $
          "Incorrectly classified instances "
            <> intToText (totalInstances - correctInstances)
            <> "\t"
            <> realFloatToText percentIncorrectInstances
            <> "%"
      liftIO $
        Text.putStrLn $
          "Total number of instances "
            <> intToText totalInstances
            <> "\n"

      liftIO $ Text.putStrLn "=== Detailed Accuracy By Class ===\n"

      liftIO $ Text.putStrLn $ tableEvaluationResults $ calcEvaluationResults dataset

numCorrectlyClassified ::
  Dataset (AlgorithmResult a) ->
  (Integer, Integer)
numCorrectlyClassified (Dataset d) = foldr countCorrectlyClassified (0, 0) d
  where
    countCorrectlyClassified result acc =
      let actualValue = Dataset.output result
          algoValue = predictedValue $ Dataset.features result
       in if actualValue == algoValue
            then (fst acc + 1, snd acc + 1)
            else (fst acc, snd acc + 1)

calcEvaluationResults ::
  Dataset (AlgorithmResult a) ->
  Map Text EvaluationResults
calcEvaluationResults (Dataset results) =
  MMap.getMonoidalMap $ foldr combineResults MMap.empty results

{-
if NA + NA -> tp NA += 1
if NA + Q* -> fn NA += 1 && fp Q* += 1
if Q* + NA -> fp NA += 1 && fn Q* += 1
if Q1 + Q1 -> tp Q* += 1
if Q1 + Q2 -> fp Q* += 1 && fn Q* += 1
-}
combineResults ::
  DatasetValue (AlgorithmResult a) ->
  MonoidalMap Text EvaluationResults ->
  MonoidalMap Text EvaluationResults
combineResults result acc
  | isNothing actualValue && isNothing algoValue =
    acc <> MMap.fromList [("NA", EvaluationResults 1 0 0)]
  | isNothing actualValue && isJust algoValue =
    acc <> MMap.fromList [("NA", EvaluationResults 0 0 1)]
      <> MMap.fromList
        [("Q*", EvaluationResults 0 1 0)]
  | isJust actualValue && isNothing algoValue =
    acc <> MMap.fromList [("NA", EvaluationResults 0 1 0)]
      <> MMap.fromList
        [("Q*", EvaluationResults 0 0 1)]
  | actualValue == algoValue && isJust actualValue && isJust algoValue =
    acc <> MMap.fromList [("Q*", EvaluationResults 1 0 0)]
  | otherwise =
    acc <> MMap.fromList [("Q*", EvaluationResults 0 1 1)]
  where
    actualValue = Dataset.output result
    algoValue = predictedValue $ Dataset.features result

-- | Produces table such as follow:
-- Class TP FP FN Precision Recall  F1
--  NA   1  1  0    0.50    1.00  0.67
--  Q*   0  1  1    0.00    0.00  0.00
tableEvaluationResults :: Map Text EvaluationResults -> Text
tableEvaluationResults results =
  tabl
    EnvAscii
    DecorNone
    DecorNone
    (map (const AlignCentre) headers)
    ([headers] <> getTextRows results)
  where
    headers = ["Class", "TP", "FP", "FN", "Precision", "Recall", "F1"]
    getTextRows = map getTableRow . Map.toList

getTableRow :: (Text, EvaluationResults) -> [Text]
getTableRow (cls, result) =
  let precision = calcPrecision result
      recall = calcRecall result
      f1 = calcF1 precision recall
   in [ cls,
        intToText $ tp result,
        intToText $ fp result,
        intToText $ fn result,
        realFloatToText precision,
        realFloatToText recall,
        realFloatToText f1
      ]
  where
    calcPrecision :: EvaluationResults -> Double
    calcPrecision evalResults =
      let tpIntegral = fromIntegral $ tp evalResults
          fpIntegral = fromIntegral $ fp evalResults
       in tpIntegral / (tpIntegral + fpIntegral)

    calcRecall :: EvaluationResults -> Double
    calcRecall evalResults =
      let tpIntegral = fromIntegral $ tp evalResults
          fnIntegral = fromIntegral $ fn evalResults
       in tpIntegral / (tpIntegral + fnIntegral)

    calcF1 precision recall = 2 * precision * recall / (precision + recall)

realFloatToText :: RealFloat a => a -> Text.Text
realFloatToText =
  Text.toStrict . Text.toLazyText . Text.formatRealFloat Text.Fixed (Just 2)

intToText :: Integral a => a -> Text.Text
intToText = Text.toStrict . Text.toLazyText . Text.decimal
