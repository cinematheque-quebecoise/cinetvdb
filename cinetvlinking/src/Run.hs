{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import
import qualified Run.CountryLinking as CountryLinking
import qualified Run.FilmoLinking as FilmoLinking
import qualified Run.NomLinking.Preprocessing as NomLinking
import qualified Run.NomLinking.Evaluation as NomLinking
import qualified Run.NomLinking.Apply as NomLinking

run :: RIO App ()
run = do
  pool <- fmap getDbPool ask

  command <- fmap (optionsCommand . appOptions) ask
  case command of
     CountryLinking -> CountryLinking.run pool
     FilmoLinking willAnnotateAll -> FilmoLinking.run willAnnotateAll
     NomLinking nomLinkingCmd ->
       case nomLinkingCmd of
         NomLinkingPreprocess size validationRatio -> NomLinking.preprocess size validationRatio
         NomLinkingEvaluation isTesting -> NomLinking.applyAlgorithmToAnnotatedData isTesting
         NomLinkingEvaluationResults isTesting -> NomLinking.evaluateAlgorithm isTesting
         NomLinkingApply willAnnotateAll -> NomLinking.applyAlgorithm willAnnotateAll

