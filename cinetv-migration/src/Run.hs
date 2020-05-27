{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Run (run) where

import Import
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Database.CineTv.Model
import Database.Esqueleto hiding (get)
import Database.Persist.Sqlite (SqliteConf(..), runSqlite, runMigration, runSqlPersistMPool)
import Database.Persist (entityVal, Entity, createPoolConfig)
import Data.Pool (Pool)
import Data.Conduit (await,yield,leftover,Conduit, ConduitT,($$),(=$), (.|), runConduit)
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Resource
import Text.RE.TDFA.Text
import System.Directory
import System.FilePath (joinPath)
import System.IO.Error (IOError, isPermissionError, isDoesNotExistError)
import Prelude (putStrLn)

run :: RIO App ()
run = do
  sqliteDbPath <- fmap (optionsSqliteDbPath . appOptions) ask
  outputDir <- fmap (optionsOuputDir . appOptions) ask

  runCineTvMigration sqliteDbPath outputDir

runCineTvMigration :: Text -- ^ CineTV SQLite database path
                   -> Text -- ^ New SQLite database path
                   -> RIO App ()
runCineTvMigration sqliteDbPath outputDir = do
  case matchedText $ sqliteDbPath ?=~ [re|[0-9]+-[0-9]+-[0-9]+|] of
    Just date -> do
      let newSqliteDbFname = "cinetv-" <> date <> "-publique.db"
      let newSqliteDbPath = ( Text.pack
                            . joinPath
                            . fmap Text.unpack
                            ) [outputDir, newSqliteDbFname]

      removeOldDatabase newSqliteDbPath

      logInfo $ display $ "Migrating SQLite database "
                       <> sqliteDbPath
                       <> " to "
                       <> newSqliteDbPath
      migrateDatabase sqliteDbPath newSqliteDbPath

    Nothing -> do
      logError "The SQLite file must contain a date in the format YYYY-MM-DD"
      exitFailure

removeOldDatabase :: (HasLogFunc env)
                  => Text -- ^ New SQLite database path
                  -> RIO env ()
removeOldDatabase newSqliteDbPath = do
  _ <- liftIO $ tryIO $ removeFile $ Text.unpack newSqliteDbPath
  return ()

migrateDatabase :: (HasLogFunc env)
                => Text -- ^ CineTV SQLite database path
                -> Text -- ^ New SQLite database path
                -> RIO env ()
migrateDatabase sqliteDbPath newSqliteDbPath = do
  ipool <- liftIO $ createPoolConfig (SqliteConf sqliteDbPath 1)
  opool <- liftIO $ createPoolConfig (SqliteConf newSqliteDbPath 1)

  liftIO $ flip runSqlPersistMPool opool $ do
    runMigration migrateAll

  -- countries <- liftIO $ getCountries ipool
  -- liftIO $ flip runSqlPersistMPool opool $ do
  --   mapM_ (\c -> insertKey (entityKey c) (entityVal c)) countries

  logInfo "Migrating Pays table..."
  liftIO $ flip runSqlPersistMPool ipool $ do
    runConduit $ selectSource (distinct $ from $ \pays -> return pays) .| (printT opool)

    -- let source = selectSource (distinct $ from $ \pays -> return pays)
    -- liftIO $ flip runSqlPersistMPool opool $ do
    --   runMigration migrateAll
    --   runConduit $ source .| CL.mapM_ (\e -> insertKey (entityKey e) (entityVal e))

-- printT :: (Monad m, MonadIO m)
--        => Pool SqlBackend
--        -> ConduitT Pays Void m ()
-- printT pool = CL.mapM_ (liftIO . Text.putStrLn . paysTerme)

printT :: (Monad m, MonadIO m)
       => Pool SqlBackend
       -> ConduitT (Entity Pays) Void m ()
printT pool = CL.mapM_ (\e -> liftIO $ flip runSqlPersistMPool pool $ insertKey (entityKey e) (entityVal e))
-- printT pool = CL.mapM_ (\e -> liftIO $ flip runSqlPersistMPool pool $ runMigration migrateAll >> insertKey (entityKey e) (entityVal e))

-- migrateCountries :: (HasDbPool env)
--                  => RIO env ()
-- migrateCountries = do
--   env <- ask
--   let pool = getDbPool env
--   countries <- liftIO $ getCountries pool
--   runSqlite "cinetv-pub.db" $ do
--     runMigration migrateAll
--     mapM_ (\c -> insertKey (entityKey c) (entityVal c)) countries
--   return ()

--   where
--     getCountries :: Pool SqlBackend -> IO [Entity Pays]
--     getCountries pool = do
--       liftIO $ flip runSqlPersistMPool pool $ do
--         select $
--           distinct $
--           from $ \pays -> do
--           return pays

-- migrateCountries :: (HasDbPool env)
--                  => RIO env ()
-- migrateCountries = do
--   env <- ask
--   let pool = getDbPool env
--   countries <- liftIO $ getCountries pool
--   runSqlite "cinetv-pub.db" $ do
--     runMigration migrateAll
--     mapM_ (\c -> insertKey (entityKey c) (entityVal c)) countries
--   return ()

--   where
--     getCountries :: Pool SqlBackend -> IO [Entity Pays]
--     getCountries pool = do
--       countryResults <- liftIO $ flip runSqlPersistMPool pool $ do
--         select $
--           distinct $
--           from $ \pays -> do
--           return pays

--       return countryResults

getCountries :: Pool SqlBackend
             -> IO [Entity Pays]
getCountries pool = do
  liftIO $ flip runSqlPersistMPool pool $ do
    select $
      distinct $
      from $ \pays -> do
      return pays
