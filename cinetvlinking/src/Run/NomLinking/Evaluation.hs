{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Run.NomLinking.Evaluation (applyAlgorithmToAnnotatedData, evaluateAlgorithm) where

import Import hiding (NomLinkingCommand(..))
import Run.NomLinking.Algorithm (runAlgorithm)
import Types.AlgorithmResult (AlgorithmResult(..), readAlgorithmTrainResults)
import Types.AnnotatedFeatures
import Types.PersonFeatures
import qualified Data.ByteString.Lazy as BL
import Data.Csv (defaultEncodeOptions, EncodeOptions(..))
import Data.Csv.Incremental (encodeDefaultOrderedByNameWith, encodeNamedRecord)
import Data.Csv.Streaming (Records)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text (toStrict)
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy.Builder as Text
import qualified Data.Text.Lazy.Builder.Int as Text
import qualified Data.Text.Lazy.Builder.RealFloat as Text
import Pipes ((>->), await, Consumer)
import qualified Pipes as Pipes (runEffect, each)
import qualified Pipes.Prelude as Pipes (map, mapM)
import Text.Tabl (tabl, Environment(EnvAscii), Decoration(..), Alignment(..))
import qualified Data.Map as Map
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MMap

type IsTestingMode = Bool

data EvaluationResults = EvaluationResults
  { tp :: Integer -- ^ Number of true positives
  , fp :: Integer -- ^ Number of false positives
  , fn :: Integer -- ^ Number of false negatives
  }

instance Semigroup EvaluationResults where
  (<>) (EvaluationResults tp1 fp1 fn1) (EvaluationResults tp2 fp2 fn2) =
    EvaluationResults (tp1 + tp2) (fp1 + fp2) (fn1 + fn2)

instance Monoid EvaluationResults where
  mempty = EvaluationResults 0 0 0

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
evaluateAlgorithm :: IsTestingMode -> RIO App ()
evaluateAlgorithm isTesting = do
  logInfo $ "Evaluating algorithm on "
         <> (if isTesting then "test" else "validation")
         <> " set...\n"

  let fpath = if isTesting
              then "data/results-test.csv"
              else "data/results-val.csv"
  eitherResults <- readAlgorithmTrainResults fpath
  case eitherResults of
    Left errMsg -> logError $ display $ Text.pack errMsg
    Right (_, records) -> showEvaluationResults records

showEvaluationResults :: Records (AlgorithmResult (AnnotatedFeatures PersonFeatures)) -> RIO App ()
showEvaluationResults results = do
  let (correctInstances, totalInstances) = numCorrectlyClassified results
  let percentCorrectInstances =
        fromIntegral correctInstances * 100 / fromIntegral totalInstances :: Double
  let percentIncorrectInstances = 100.0 - percentCorrectInstances

  liftIO $ Text.putStrLn "=== Summary ===\n"
  liftIO $ Text.putStrLn $ "Correctly classified instances "
                        <> intToText correctInstances
                        <> "\t"
                        <> realFloatToText percentCorrectInstances
                        <> "%"
  liftIO $ Text.putStrLn $ "Incorrectly classified instances "
                        <> (intToText $ totalInstances - correctInstances)
                        <> "\t"
                        <> realFloatToText percentIncorrectInstances
                        <> "%"
  liftIO $ Text.putStrLn $ "Total number of instances "
                        <> intToText totalInstances
                        <> "\n"

  liftIO $ Text.putStrLn $ "=== Detailed Accuracy By Class ===\n"

  liftIO $ Text.putStrLn $ tableEvaluationResults $ calcEvaluationResults results

numCorrectlyClassified :: Records (AlgorithmResult (AnnotatedFeatures PersonFeatures))
                       -> (Integer, Integer)
numCorrectlyClassified results =
  foldr countCorrectlyClassified (0, 0) results
  where
    countCorrectlyClassified result acc =
      let actualValue = output $ input result
          algoValue = predictedValue result
       in if actualValue == algoValue
          then (fst acc + 1, snd acc + 1)
          else (fst acc, snd acc + 1)

calcEvaluationResults :: Records (AlgorithmResult (AnnotatedFeatures PersonFeatures))
                      -> Map Text EvaluationResults
calcEvaluationResults results =
  MMap.getMonoidalMap $ foldr combineResults (MMap.empty) results

{-
if NA + NA -> tp NA += 1
if NA + Q* -> fn NA += 1 && fp Q* += 1
if Q* + NA -> fp NA += 1 && fn Q* += 1
if Q1 + Q1 -> tp Q* += 1
if Q1 + Q2 -> fp Q* += 1 && fn Q* += 1
-}
combineResults :: (AlgorithmResult (AnnotatedFeatures PersonFeatures))
               -> MonoidalMap Text EvaluationResults
               -> MonoidalMap Text EvaluationResults
combineResults result acc =
  if actualValue == "NA" && algoValue == "NA"
  then acc <> MMap.fromList [("NA", EvaluationResults 1 0 0)]
  else if actualValue == "NA" && algoValue /= "NA"
  then acc
    <> MMap.fromList [("NA", EvaluationResults 0 0 1)]
    <> MMap.fromList [("Q*", EvaluationResults 0 1 0)]
  else if actualValue /= "NA" && algoValue == "NA"
  then acc
    <> MMap.fromList [("NA", EvaluationResults 0 1 0)]
    <> MMap.fromList [("Q*", EvaluationResults 0 0 1)]
  else if actualValue == algoValue
       && actualValue /= "NA"
       && algoValue /= "NA"
  then acc <> MMap.fromList [("Q*", EvaluationResults 1 0 0)]
  else acc <> MMap.fromList [("Q*", EvaluationResults 0 1 1)]
    where
      actualValue = output $ input result
      algoValue = predictedValue result

{- | Produces table such as follow:
Class TP FP FN Precision Recall  F1
  NA   1  1  0    0.50    1.00  0.67
  Q*   0  1  1    0.00    0.00  0.00
-}
tableEvaluationResults :: Map Text EvaluationResults -> Text
tableEvaluationResults results =
  tabl EnvAscii
       DecorNone
       DecorNone
       (map (\_ -> AlignCentre) headers)
       ([headers] <> getTextRows results)
  where
    headers = ["Class", "TP", "FP", "FN", "Precision", "Recall", "F1"]
    getTextRows = (map getTableRow . Map.toList)

getTableRow :: (Text, EvaluationResults) -> [Text]
getTableRow (cls, result) =
  let precision = calcPrecision result
      recall = calcRecall result
      f1 = calcF1 precision recall
  in [ cls
     , intToText $ tp $ result
     , intToText $ fp $ result
     , intToText $ fn $ result
     , realFloatToText $ precision
     , realFloatToText $ recall
     , realFloatToText $ f1
     ]

  where
    calcPrecision :: EvaluationResults -> Double
    calcPrecision evalResults =
      let tpIntegral = fromIntegral $ tp evalResults
          fpIntegral = fromIntegral $ fp evalResults
       in tpIntegral/ (tpIntegral + fpIntegral)

    calcRecall :: EvaluationResults -> Double
    calcRecall evalResults =
      let tpIntegral = fromIntegral $ tp evalResults
          fnIntegral = fromIntegral $ fn evalResults
       in tpIntegral/ (tpIntegral + fnIntegral)

    calcF1 precision recall = 2 * precision * recall / (precision + recall)

realFloatToText :: RealFloat a => a -> Text.Text
realFloatToText = Text.toStrict
                . Text.toLazyText
                . Text.formatRealFloat Text.Fixed (Just 2)

intToText :: Integral a => a -> Text.Text
intToText = Text.toStrict . Text.toLazyText . Text.decimal


applyAlgorithmToAnnotatedData :: IsTestingMode -> RIO App ()
applyAlgorithmToAnnotatedData isTesting = do
  logInfo $ "Applying algorithm on "
         <> (if isTesting then "test" else "validation")
         <> " set..."

  let fpath = if isTesting
              then "data/people-annotated-test.csv"
              else "data/people-annotated-val.csv"
  eitherRecords <- readAnnotatedPeopleData fpath

  let outputfpath = if isTesting
              then "data/results-test.csv"
              else "data/results-val.csv"
  case eitherRecords of
    Left errMsg -> logError $ display $ Text.pack errMsg
    Right (_, records) -> saveEvaluationResults outputfpath records

  logInfo $ "Finished applying algorithm to "
         <> (if isTesting then "test" else "validation")
         <> " set."

saveEvaluationResults :: FilePath
                      -> Records (AnnotatedFeatures PersonFeatures)
                      -> RIO App ()
saveEvaluationResults fpath records = do
  -- env <- ask
  -- let creditsFetcher = createCineTvCreditsFetcher $ getDbPool env
  writeFileUtf8 fpath "id,firstname,lastname,notes,fonctions,output,actual\r\n"
  Pipes.runEffect $
        Pipes.each records
    -- >-> Pipes.mapM (\r -> (AlgorithmResult r) <$> runAlgorithm r)
    >-> Pipes.mapM (\r -> do predictedMaybe <- runAlgorithm (features r)
                             case predictedMaybe of
                               Just predicted -> do
                                 let qid = Text.replace "http://www.wikidata.org/entity/" "" predicted
                                 return $ AlgorithmResult r qid
                               Nothing -> return $ AlgorithmResult r "NA"
                   )
    >-> Pipes.map ((encodeDefaultOrderedByNameWith myOptions) . encodeNamedRecord)
    >-> appendFileConsumer fpath

  return ()

  where myOptions = defaultEncodeOptions {
    encIncludeHeader = False
  }

appendFileConsumer :: (MonadIO m)
                   => FilePath
                   -> Consumer BL.ByteString m ()
appendFileConsumer fpath = forever $ do
  result <- Pipes.await
  liftIO $ BL.appendFile fpath result

