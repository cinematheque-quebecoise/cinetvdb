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
                    <> help "File path of the CineTV SQLite database file"
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

  let cinetvDbPath = optionsSqliteDbPath options
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
     command "nom" ((NomLinking <$> parseLinking) `withInfo` "Link people (Nom table) to Wikidata.")
  <> command "filmo" ((FilmoLinking <$> parseLinking) `withInfo` "Link works (Filmo table) to Wikidata.")
  <> command "org" ((OrgLinking <$> parseLinking) `withInfo` "Link organisations (Sujet/Organisme table) to Wikidata.")

parseLinking :: Parser LinkingCommand
parseLinking = subparser $
     command "preprocess" (parseLinkingPreprocess `withInfo` "Preprocess data for linking.")
  <> command "evaluate" (parseLinkingEvaluate `withInfo` "Apply algorithm on annotated dataset.")
  <> command "evaluate-result" (parseLinkingEvaluateResult `withInfo` "Evaluate algorithm on generated dataset.")
  <> command "apply" (parseLinkingApply `withInfo` "Apply algorithm on unannotated dataset.")
  <> command "interactive" (parseLinkingInteractive `withInfo` "Apply algorithm interactively.")

parseLinkingPreprocess :: Parser LinkingCommand
parseLinkingPreprocess = LinkingPreprocess
    <$> argument str (metavar "CINETV_EXT_DIR")
    <*> argument auto (metavar "TOTAL_DATA_SIZE")
    <*> argument auto (metavar "VALIDATION_DATA_RATIO")

parseLinkingEvaluate :: Parser LinkingCommand
parseLinkingEvaluate = LinkingEvaluation
  <$> switch ( long "test"
            <> short 't'
            <> help "Final testing"
             )

parseLinkingEvaluateResult :: Parser LinkingCommand
parseLinkingEvaluateResult = LinkingEvaluationResults
  <$> switch ( long "test"
            <> short 't'
            <> help "Final testing"
             )

parseLinkingApply :: Parser LinkingCommand
parseLinkingApply = LinkingApply
  <$> switch ( long "restart"
            <> short 'r'
            <> help "Algorithm application on ALL data (even if already annotated)"
             )

parseLinkingInteractive :: Parser LinkingCommand
parseLinkingInteractive = pure LinkingInteractive
