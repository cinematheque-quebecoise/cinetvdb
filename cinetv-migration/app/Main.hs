{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Import
import Run
import RIO.Process
import Options.Applicative.Simple
import qualified Paths_cinetv_migration

main :: IO ()
main = do
  (options, ()) <- simpleOptions
    $(simpleVersion Paths_cinetv_migration.version)
    "Header for command line arguments"
    "Program description, also for command line arguments"
    (Options
       <$> switch ( long "verbose"
                 <> short 'v'
                 <> help "Verbose output?"
                  )
       <*> strOption ( long "sqlitedb"
                    <> short 's'
                    <> metavar "SQLITEDBFILE"
                    <> help "File path of the Sqlite database file"
                     )
       <*> strOption ( long "dir"
                    <> short 'd'
                    <> metavar "OUTPUTDIR"
                    <> help "Output directory containing generated files"
                     )
    )
    empty
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext

  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          }
     in runRIO app run
