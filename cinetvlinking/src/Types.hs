{-# LANGUAGE NoImplicitPrelude #-}
module Types where

import RIO
import RIO.Process

import Data.Pool (Pool)
import Database.Persist.Class (PersistConfigPool)
import Database.Persist.Sqlite (SqliteConf(..), SqlBackend)

data Command = FilmoLinking !LinkingCommand
             | NomLinking !LinkingCommand
             | OrgLinking !LinkingCommand

data LinkingCommand = LinkingPreprocess !FilePath !Integer !Double
                    | LinkingEvaluation !Bool
                    | LinkingEvaluationResults !Bool
                    | LinkingApply !Bool
                    | LinkingInteractive

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  , optionsCommand :: !Command
  , optionsSqliteDbPath :: !FilePath
  , optionsOutputDir :: !FilePath
  }

data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !Options
  -- Add other app-specific configuration information here
  , appDbPool :: !(PersistConfigPool SqliteConf)
  }

class HasDbPool env where
  getDbPool :: env -> Pool SqlBackend

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })
instance HasDbPool App where
  getDbPool = appDbPool
