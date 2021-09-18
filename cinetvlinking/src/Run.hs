{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run
  ( run,
  )
where

import EntityLinking.Evaluation (applyAlgorithmToAnnotatedData, showEvaluationResults)
import qualified EntityLinking.Preprocessing as EntityLinking
import Import
import EntityLinking.Nom.Types
import EntityLinking.Filmo.Types (FilmoFeatures)
import RIO.FilePath (joinPath)
import EntityLinking.Apply (writeDisambiguatedEntities)
import EntityLinking.Interactive (promptNomId, interact, promptFilmoId)

run :: RIO App ()
run = do
  command <- fmap (optionsCommand . appOptions) ask
  case command of
    FilmoLinking linkingCmd -> case linkingCmd of
      LinkingPreprocess cinetvExtDir size validationRatio -> do
        logInfo "Creating validation and test datasets..."
        preprocessFilmoEntities cinetvExtDir size validationRatio
      LinkingEvaluation isTesting ->
        applyAlgorithmToAnnotatedFilmoEntities isTesting
      LinkingEvaluationResults isTesting ->
          showFilmoEntityAlgoEvalResults isTesting
      LinkingApply willAnnotateAll ->
        disambiguateFilmo willAnnotateAll
      LinkingInteractive -> interact promptFilmoId
    NomLinking linkingCmd -> case linkingCmd of
      LinkingPreprocess cinetvExtDir size validationRatio -> do
        logInfo "Creating validation and test datasets..."
        preprocessPersonEntities cinetvExtDir size validationRatio
      LinkingEvaluation isTesting ->
        applyAlgorithmToAnnotatedPersonEntities isTesting
      LinkingEvaluationResults isTesting ->
        showPersonEntityAlgoEvalResults isTesting
      LinkingApply willAnnotateAll ->
        disambiguateNom willAnnotateAll
      LinkingInteractive -> interact promptNomId
    OrgLinking linkingCmd -> case linkingCmd of
      LinkingPreprocess cinetvExtDir size validationRatio -> do
        logInfo "Creating validation and test datasets..."
        -- preprocessOrgEntities cinetvExtDir size validationRatio
      LinkingEvaluation isTesting -> undefined -- applyAlgorithmToAnnotatedPersonEntities isTesting
      LinkingEvaluationResults isTesting -> undefined -- showPersonEntityAlgoEvalResults isTesting
      LinkingApply willAnnotateAll -> undefined -- disambiguateNom willAnnotateAll
      LinkingInteractive -> interact promptNomId

-- | Sample a certain number of instances, split then into training and
-- validation sets, and save them to files.
preprocessPersonEntities ::
  -- | Dataset containing annotated entity instances
  FilePath ->
  -- | Number of instances to sample
  Integer ->
  -- | Ratio for splitting instances to training and validation sets
  Double ->
  RIO App ()
preprocessPersonEntities cinetvExtDir sampleSize validationRatio = do
  let nomAnnotatedCsvFpath = cinetvExtDir <> "/Nom_LienWikidata.csv"
  let outputBaseFpath = "people-annotated"
  EntityLinking.preprocess
    (Proxy @PersonFeatures)
    nomAnnotatedCsvFpath
    sampleSize
    validationRatio
    outputBaseFpath

-- | Sample a certain number of instances, split then into training and
-- validation sets, and save them to files.
preprocessFilmoEntities ::
  -- | Dataset containing annotated entity instances
  FilePath ->
  -- | Number of instances to sample
  Integer ->
  -- | Ratio for splitting instances to training and validation sets
  Double ->
  RIO App ()
preprocessFilmoEntities cinetvExtDir sampleSize validationRatio = do
  let filmoAnnotatedCsvFpath = cinetvExtDir <> "/Filmo_LienWikidata.csv"
  let outputBaseFpath = "filmo-annotated"
  EntityLinking.preprocess
    (Proxy @FilmoFeatures)
    filmoAnnotatedCsvFpath
    sampleSize
    validationRatio
    outputBaseFpath

applyAlgorithmToAnnotatedPersonEntities :: Bool -> RIO App ()
applyAlgorithmToAnnotatedPersonEntities isTesting = do
  logInfo $
    "Applying algorithm on "
      <> (if isTesting then "test" else "validation")
      <> " set..."

  outputDir <- optionsOutputDir . appOptions <$> ask
  let inputFpath =
        if isTesting
          then outputDir <> "/people-annotated-testset.csv"
          else outputDir <> "/people-annotated-validationset.csv"

  let outputFpath =
        if isTesting
          then outputDir <> "/people-algorithm-results-testset.csv"
          else outputDir <> "/people-algorithm-results-validationset.csv"

  applyAlgorithmToAnnotatedData (Proxy @PersonFeatures) inputFpath outputFpath

  logInfo $
    "Finished applying algorithm to "
      <> (if isTesting then "test" else "validation")
      <> " set."

applyAlgorithmToAnnotatedFilmoEntities :: Bool -> RIO App ()
applyAlgorithmToAnnotatedFilmoEntities isTesting = do
  logInfo $
    "Applying algorithm on "
      <> (if isTesting then "test" else "validation")
      <> " set..."

  outputDir <- optionsOutputDir . appOptions <$> ask
  let inputFpath =
        if isTesting
          then outputDir <> "/filmo-annotated-testset.csv"
          else outputDir <> "/filmo-annotated-validationset.csv"

  let outputFpath =
        if isTesting
          then outputDir <> "/filmo-algorithm-results-testset.csv"
          else outputDir <> "/filmo-algorithm-results-validationset.csv"

  applyAlgorithmToAnnotatedData (Proxy @FilmoFeatures) inputFpath outputFpath

  logInfo $
    "Finished applying algorithm to "
      <> (if isTesting then "test" else "validation")
      <> " set."

showPersonEntityAlgoEvalResults :: Bool -> RIO App ()
showPersonEntityAlgoEvalResults isTesting = do
  logInfo $
    "Evaluating algorithm on "
      <> (if isTesting then "test" else "validation")
      <> " set...\n"

  outputDir <- optionsOutputDir . appOptions <$> ask
  let inputFpath =
        if isTesting
          then outputDir <> "/people-algorithm-results-testset.csv"
          else outputDir <> "/people-algorithm-results-validationset.csv"

  showEvaluationResults (Proxy @PersonFeatures) inputFpath

showFilmoEntityAlgoEvalResults :: Bool -> RIO App ()
showFilmoEntityAlgoEvalResults isTesting = do
  logInfo $
    "Evaluating algorithm on "
      <> (if isTesting then "test" else "validation")
      <> " set...\n"

  outputDir <- optionsOutputDir . appOptions <$> ask
  let inputFpath =
        if isTesting
          then outputDir <> "/filmo-algorithm-results-testset.csv"
          else outputDir <> "/filmo-algorithm-results-validationset.csv"

  showEvaluationResults (Proxy @FilmoFeatures) inputFpath

disambiguateNom :: Bool -> RIO App ()
disambiguateNom willAnnotateAll = do
  logInfo "Linking Nom table to Wikidata..."

  outputdir <- fmap (optionsOutputDir . appOptions) ask
  let outputFpath = joinPath [outputdir, "Nom_LienWikidata.csv"]

  writeDisambiguatedEntities (Proxy @PersonFeatures) willAnnotateAll outputFpath

disambiguateFilmo :: Bool -> RIO App ()
disambiguateFilmo willAnnotateAll = do
  logInfo "Linking Filmo table to Wikidata..."

  outputdir <- fmap (optionsOutputDir . appOptions) ask
  let outputFpath = joinPath [outputdir, "Filmo_LienWikidata.csv"]

  writeDisambiguatedEntities (Proxy @FilmoFeatures) willAnnotateAll outputFpath

