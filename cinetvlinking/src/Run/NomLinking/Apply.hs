{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

{-# LANGUAGE QuasiQuotes #-}

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
import System.FilePath (joinPath)
import System.IO.Error (isDoesNotExistError)
import System.Directory (doesFileExist)

-- import qualified Data.Text as Text
-- import qualified Data.Text.IO as Text
-- import qualified Data.Text.Internal.Builder as Text
-- import qualified Data.Text.Lazy.Encoding as Text (encodeUtf8)
-- import qualified Data.Text.Read as Text
-- import Network.HTTP.Client
-- import Database.SPARQL.Protocol.Client
-- import NeatInterpolation
-- import Prelude (print)
-- import qualified Data.ByteString.Lazy.Char8 as BS
-- import Database.HSparql.Connection
-- import Database.HSparql.QueryGenerator

-- getIRI :: RDFTerm -> Maybe Text
-- getIRI (IRI iri) = Just iri
-- getIRI _ = Nothing

applyAlgorithm :: Bool -> RIO App ()
applyAlgorithm willAnnotateAll = do
  logInfo $ "Linking Nom table to Wikidata..."

  -- let sparqlEndpoint = "https://query.wikidata.org/bigdata/namespace/wdq/sparql?query"
  -- response <- liftIO $ select sparqlEndpoint (textToLazyBs $ query)
  -- liftIO $ print $ responseStatus response
  -- let (SelectResult results) = responseBody response

  -- liftIO $ Text.putStrLn query
  -- liftIO $ BS.putStrLn $ textToLazyBs query
  -- forM_ results $ \r -> do
  --   liftIO $ print r

  -- liftIO $ Text.putStrLn $ Text.pack $ createSelectQuery queryhsparql
  -- result <- liftIO $ selectQueryRaw sparqlEndpoint $ Text.unpack query
  -- liftIO $ print result
  -- liftIO $ mapM print result
  -- return ()

  -- where
  --   sparqlEndpoint = "http://query.wikidata.org/sparql"
  --   -- sparqlEndpoint = "http://dbpedia.org/sparql"
  --   textToLazyBs = Text.encodeUtf8 . Text.toLazyText . Text.fromText
  --   query =
  --     [text|PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
-- PREFIX wd: <http://www.wikidata.org/entity/>
-- PREFIX wdt: <http://www.wikidata.org/prop/direct/>
-- SELECT DISTINCT ?item
-- WHERE
-- {
  -- { ?item rdfs:label "Michel Côté"@fr. } UNION
  -- { ?item rdfs:label "Michel Côté"@en. }
-- } LIMIT 10
  --     |]
  --   queryhsparql :: Query SelectQuery
  --   queryhsparql = do
  --     rdfs <- prefix "rdfs" (iriRef "http://www.w3.org/2000/01/rdf-schema#")

  --     x <- var
  --     triple_ x (rdfs .:. "label") (("Michel Brault", "fr") :: (Text, Text))

  --     selectVars [x]

  outputdir <- fmap (optionsOutputDir . appOptions) ask

  allPersonFeatures <- fmap getDbPool ask >>= (liftIO . getPersonFeatures)

  let fpath = joinPath [outputdir, "Nom_LienWikidata.csv"]

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
  pooledForConcurrently_ dataToAnnotate $ \personFeatures -> do
    personOutput <- predictPersonFeaturesOutput personFeatures
    let result = (encodeDefaultOrderedByNameWith myOptions) $ encodeNamedRecord personOutput
    liftIO $ BL.appendFile fpath $ result

  where
    myOptions = defaultEncodeOptions { encIncludeHeader = False }

predictPersonFeaturesOutput :: (HasLogFunc env, HasDbPool env)
                            => PersonFeatures
                            -> RIO env (AlgorithmResult PersonFeatures)
predictPersonFeaturesOutput personFeatures = do
  logInfo $ display $ "Linking NomID " <> (Text.pack . show . personId) personFeatures <> "..."
  predictedValue <- fmap (fromMaybe "NA") $ runAlgorithm personFeatures
  logInfo $ display $ " to " <> predictedValue
  return $ AlgorithmResult personFeatures predictedValue

recordsToList :: Records a -> [a]
recordsToList records = foldr (\p acc -> mconcat [[p], acc]) [] records
