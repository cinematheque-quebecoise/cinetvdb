{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run
  ( run
  )
where

import           Import
import qualified Run.FilmoLinking             as FilmoLinking
import qualified Run.NomLinking.Apply         as NomLinking
import qualified Run.NomLinking.Evaluation    as NomLinking
import qualified Run.NomLinking.Interactive   as NomLinking
import qualified Run.NomLinking.Preprocessing as NomLinking

run :: RIO App ()
run = do
  pool    <- fmap getDbPool ask

  command <- fmap (optionsCommand . appOptions) ask
  case command of
    FilmoLinking willAnnotateAll -> FilmoLinking.run willAnnotateAll
    NomLinking   nomLinkingCmd   -> case nomLinkingCmd of
      NomLinkingPreprocess cinetvExtDir size validationRatio ->
        NomLinking.preprocess cinetvExtDir size validationRatio
      NomLinkingEvaluation isTesting ->
        NomLinking.applyAlgorithmToAnnotatedData isTesting
      NomLinkingEvaluationResults isTesting ->
        NomLinking.evaluateAlgorithm isTesting
      NomLinkingApply willAnnotateAll ->
        NomLinking.applyAlgorithm willAnnotateAll
      NomLinkingInteractive -> NomLinking.interact
