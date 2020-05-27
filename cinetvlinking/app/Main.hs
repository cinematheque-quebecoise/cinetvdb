{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Import
import Run
import RIO.Process
import qualified RIO.Text as Text
import Options.Applicative.Simple
import qualified Paths_cinetvlinking
import Database.Persist (createPoolConfig)
import Database.Persist.Sqlite (SqliteConf(..))
import System.Directory (doesFileExist)

-- Where withInfo is just a convenience function to add --help support given
-- a parser and description
withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

main :: IO ()
main = do
  (options, ()) <- simpleOptions
    $(simpleVersion Paths_cinetvlinking.version)
    "Header for command line arguments"
    "Program description, also for command line arguments"
    (Options
       <$> switch ( long "verbose"
                 <> short 'v'
                 <> help "Verbose output?"
                  )
       <*> parseCommand
       <*> strOption ( long "cinetvdb"
                    <> short 'd'
                    <> metavar "CINETVDB"
                    <> help "File path of the CineTV Sqlite database file"
                     )
       <*> strOption ( long "outputdir"
                    <> short 'o'
                    <> metavar "OUTPUTDIR"
                    <> help "Output directory for saved files."
                     )
    )
    empty
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext

  -- let sqliteDbPath = optionsSqliteDbPath options
  -- pool <- createPoolConfig (SqliteConf sqliteDbPath 1)

  let cinetvDbPath = optionsSqliteDbPath $ options
  cinetvDbPathExists <- doesFileExist cinetvDbPath
  unless cinetvDbPathExists $ error $ "Filepath " <> cinetvDbPath <> " does not exists!"
  pool <- createPoolConfig (SqliteConf (Text.pack cinetvDbPath) 1)

  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          , appDbPool = pool
          }
     in runRIO app run

parseCommand :: Parser Command
parseCommand = subparser $
     command "nom" ((NomLinking <$> parseNomLinking) `withInfo` "Link Nom table to Wikidata.")
  <> command "pays" ((pure CountryLinking) `withInfo` "Link Pays table to Wikidata.")
  <> command "filmo" (parseFilmoLinking `withInfo` "Link Filmo table to Wikidata.")

parseNomLinking :: Parser NomLinkingCommand
parseNomLinking = subparser $
     command "preprocess" (parseNomLinkingPreprocess `withInfo` "Preprocess data for linking Nom table.")
  <> command "evaluate" (parseNomLinkingEvaluate `withInfo` "Apply algorithm on annotated dataset.")
  <> command "evaluate-result" (parseNomLinkingEvaluateResult `withInfo` "Evaluate algorithm on generated dataset.")
  <> command "apply" (parseNomLinkingApply `withInfo` "Apply algorithm on unannotated dataset.")

parseNomLinkingPreprocess :: Parser NomLinkingCommand
parseNomLinkingPreprocess = NomLinkingPreprocess
    <$> argument auto (metavar "TOTAL_DATA_SIZE")
    <*> argument auto (metavar "VALIDATION_DATA_RATIO")

parseNomLinkingEvaluate :: Parser NomLinkingCommand
parseNomLinkingEvaluate = NomLinkingEvaluation
  <$> switch ( long "test"
            <> short 't'
            <> help "Final testing"
             )

parseNomLinkingEvaluateResult :: Parser NomLinkingCommand
parseNomLinkingEvaluateResult = NomLinkingEvaluationResults
  <$> switch ( long "test"
            <> short 't'
            <> help "Final testing"
             )

parseNomLinkingApply :: Parser NomLinkingCommand
parseNomLinkingApply = NomLinkingApply
  <$> switch ( long "restart"
            <> short 'r'
            <> help "Algorithm application on ALL data (even if already annotated)"
             )

parseFilmoLinking :: Parser Command
parseFilmoLinking = FilmoLinking
  <$> switch ( long "restart"
            <> short 'r'
            <> help "Algorithm application on ALL data (even if already annotated)"
             )
