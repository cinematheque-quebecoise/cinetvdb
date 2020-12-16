{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Run.NomLinking.Apply (applyAlgorithm) where

import Import
-- import Run.NomLinking.Algorithm (runAlgorithm)
import           EntityLinking.Nom.Wikidata         (LinkedPerson(..), linkNomId)
import Types.AlgorithmResult (AlgorithmResult(..), readAlgorithmResults)
import Types.PersonFeatures
import Database.CineTV (MonadGetPersonFeatures(..), MonadGetPersonLinkedIds(..))
-- import Run.NomLinking.Model (selectNomLienWikidataIds)
import           Types.Resource                           (ResourceUri (..))

import qualified Data.ByteString.Lazy as BL
import           Database.Esqueleto (fromSqlKey)
import qualified Data.Set as Set
import Data.Csv.Streaming (Records)
import Data.Csv (DefaultOrdered(..), defaultEncodeOptions, EncodeOptions(..), encodeByName)
import Data.Csv.Incremental (encodeDefaultOrderedByNameWith, encodeNamedRecord)
import qualified Data.Text as Text
import System.FilePath (joinPath)
import System.IO.Error (isDoesNotExistError)
import System.Directory (doesFileExist)
import qualified Data.Either as E

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
  logInfo "Linking Nom table to Wikidata..."

  outputdir <- fmap (optionsOutputDir . appOptions) ask

  -- get manually annotated data (SQLite db)
  -- get automatically classified data (CSV file)
  -- get all data (from SQLite db)
  -- if willAnnotateAll
  -- then dataToAnnotate = all data - manually annotated data
  -- else dataToAnnotate = all data - manually annotated data - automatically classified data

  manuallyAnnotatedPersonKeys <- getPersonLinkedIds
  let manuallyAnnotatedPersonIds = Set.fromList $ fromSqlKey <$> manuallyAnnotatedPersonKeys

  allPersonFeatures <- getPersonFeatures

  let fpath = joinPath [outputdir, "Nom_LienWikidata.csv"]

  e <- tryJust (guard . isDoesNotExistError) $ do
    annotatedPersonFeaturesE <- readAnnotatedPersonFeatures fpath
    case annotatedPersonFeaturesE of
      Left err -> do
        logError $ display $ Text.pack err
        exitFailure
      Right annotatedPersonFeatures -> return annotatedPersonFeatures
  let autoAnnotatedPersons = either (const []) id e

  let filteredAutoAnnotatedPersons = filter (\p -> Set.notMember (personId $ input p) manuallyAnnotatedPersonIds) autoAnnotatedPersons
  let filteredAutoAnnotatedPersonIds = Set.fromList $ personId . input <$> filteredAutoAnnotatedPersons
  when (null autoAnnotatedPersons) $ do
    let header = headerOrder (undefined :: AlgorithmResult PersonFeatures)
    liftIO $ BL.writeFile fpath $ encodeByName header filteredAutoAnnotatedPersons

  -- let dataToAnnotate = getDataToPredict willAnnotateAll allPersonFeatures annotatedPersonIds annotatedData
  let allAnnotatedPersonIds = manuallyAnnotatedPersonIds <> filteredAutoAnnotatedPersonIds
  let peopleToAnnotate = if willAnnotateAll
                     then filter (\p -> Set.notMember (personId p) manuallyAnnotatedPersonIds) allPersonFeatures
                     else filter (\p -> Set.notMember (personId p) allAnnotatedPersonIds) allPersonFeatures
  -- let annotatedDataIdsSet = foldr Set.insert Set.empty annotatedPersonIds
  --     unannotatedData = filter (\p -> Set.notMember (personId p) annotatedDataIdsSet) allPersonFeatures
  --  in if willAnnotateAll then annotatedData ++ unannotatedData else unannotatedData

  logInfo $ display $ Text.pack (show $ if willAnnotateAll then length manuallyAnnotatedPersonIds else length allAnnotatedPersonIds)
                   <> " instances are already annotated!"
  logInfo $ display $ Text.pack (show $ length peopleToAnnotate)
                   <> " will be annotated..."

  fpathExists <- liftIO $ doesFileExist fpath
  when (willAnnotateAll || not fpathExists) $ do
    let headerLine = Text.intercalate ","
                   $ fmap (decodeUtf8With (\_ _ -> Nothing))
                   $ toList
                   $ headerOrder (undefined :: AlgorithmResult PersonFeatures)
    writeFileUtf8 fpath $ headerLine <> "\r\n"

  savePredictedOutputOfPersonFeatures fpath peopleToAnnotate

readAnnotatedPersonFeatures :: (MonadIO m)
                            => FilePath
                            -> m (Either String [AlgorithmResult PersonFeatures])
readAnnotatedPersonFeatures fpath = do
  algoResultsCsv <- readAlgorithmResults fpath
  let algoResultsE = fmap (recordsToList . snd) algoResultsCsv
  return $ fmap (foldr (\p acc -> mconcat [[p], acc]) []) algoResultsE
  -- case algoResultsEither of
  --   Left err -> do
  --     logError $ display $ Text.pack err
  --     exitFailure
  --   Right annotatedDataRecords -> do
  --     return $ fmap input $ foldr (\p acc -> mconcat [[p], acc]) [] annotatedDataRecords

-- getDataToPredict :: Bool -- ^ If true, will reannotated all data!
--                  -> [PersonFeatures] -- ^ Person features from database
--                  -> [Int64] -- ^ Manually annotated person ids
--                  -> [PersonFeatures] -- ^ Already classified person features
--                  -> [PersonFeatures]
-- getDataToPredict willAnnotateAll allPersonFeatures annotatedPersonIds =
--   -- let annotatedDataIdsSet = foldr Set.insert Set.empty $ fmap personId annotatedData
--   let annotatedDataIdsSet = foldr Set.insert Set.empty annotatedPersonIds
--       unannotatedData = filter (\p -> Set.notMember (personId p) annotatedDataIdsSet) allPersonFeatures
--    in if willAnnotateAll then annotatedData ++ unannotatedData else unannotatedData

savePredictedOutputOfPersonFeatures :: (HasLogFunc env, HasDbPool env)
                                    => FilePath
                                    -> [PersonFeatures] -- ^ Data to annotate
                                    -> RIO env ()
savePredictedOutputOfPersonFeatures fpath dataToAnnotate =
  withFile fpath AppendMode $ \h ->
    pooledForConcurrently_ dataToAnnotate $ \personFeatures ->

      handle (errorHandler personFeatures) $ do
        personOutput <- predictPersonFeaturesOutput personFeatures
        let result = encodeDefaultOrderedByNameWith myOptions $ encodeNamedRecord personOutput
        liftIO $ BL.hPut h result
      -- liftIO $ BL.appendFile fpath result

  where
    errorHandler personFeatures (SomeException e) = do
      logWarn $ display $ "Runtime error when linking " <> Text.pack (show $ personId personFeatures)
      logWarn $ displayShow e

    myOptions = defaultEncodeOptions { encIncludeHeader = False }

predictPersonFeaturesOutput :: (HasLogFunc env, HasDbPool env)
                            => PersonFeatures
                            -> RIO env (AlgorithmResult PersonFeatures)
predictPersonFeaturesOutput personFeatures = do
  linkedPersonE <- linkNomId (personId personFeatures)
  let predictedPersonUri = E.fromRight "NA" $ unResourceUri . linkedPersonUri <$> linkedPersonE
  logInfo $ display $ "Linking NomID "
                   <> (Text.pack . show . personId) personFeatures
                   <> " to "
                   <> predictedPersonUri
  return $ AlgorithmResult personFeatures predictedPersonUri

recordsToList :: Records a -> [a]
recordsToList = foldr (\p acc -> mconcat [[p], acc]) []
