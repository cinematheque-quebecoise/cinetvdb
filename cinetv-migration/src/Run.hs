{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Run (run) where

import Import hiding ((^.))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Database.CineTv.Public.Model
import Database.Esqueleto hiding (get)
import Database.Persist.Sqlite (SqliteConf(..))
-- import Database.Persist.Sqlite (SqliteConf(..), runMigration, runSqlPersistMPool)
-- import Database.Persist (entityVal, Entity, createPoolConfig)
import Data.Pool (Pool)
-- import Data.Conduit (await,yield,leftover,Conduit, ConduitT,($$),(=$), (.|), runConduit)
-- import qualified Data.Conduit.List as CL
-- import Control.Monad.Trans.Resource
import Text.RE.TDFA.Text
import System.Directory
import System.FilePath (joinPath)
-- import System.IO.Error (IOError, isPermissionError, isDoesNotExistError)
-- import Prelude (putStrLn, print)

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

removeOldDatabase :: (MonadIO m)
                  => Text -- ^ New SQLite database path
                  -> m ()
removeOldDatabase newSqliteDbPath = do
  _ <- liftIO $ tryIO $ removeFile $ Text.unpack newSqliteDbPath
  return ()

migrateDatabase :: (HasLogFunc env, MonadReader env m, MonadIO m)
                => Text -- ^ CineTV SQLite database path
                -> Text -- ^ New SQLite database path
                -> m ()
migrateDatabase sqliteDbPath newSqliteDbPath = do
  ipool <- liftIO $ createPoolConfig (SqliteConf sqliteDbPath 1)
  opool <- liftIO $ createPoolConfig (SqliteConf newSqliteDbPath 1)

  liftIO $ flip runSqlPersistMPool opool $ do
    runMigration migrateAll

  -- countries <- liftIO $ getCountries ipool
  -- liftIO $ flip runSqlPersistMPool opool $ do
  --   mapM_ (\c -> insertKey (entityKey c) (entityVal c)) countries

  -- logInfo "Migrating Film_Filmo table..."
  logInfo "Fetching Film table..."
  film <- liftIO $ getFilm ipool
  logInfo "Fetching Filmo table..."
  filmo <- liftIO $ getFilmo ipool
  logInfo "Fetching Nom table..."
  nom <- liftIO $ getNom ipool
  logInfo "Fetching Sujet table..."
  sujet <- liftIO $ getSujet ipool
  logInfo "Fetching Pays table..."
  pays <- liftIO $ getPays ipool
  logInfo "Fetching Langue table..."
  langue <- liftIO $ getLangue ipool
  logInfo "Fetching Fonction table..."
  fonction <- liftIO $ getFonction ipool
  logInfo "Fetching Film_Filmo table..."
  filmFilmo <- liftIO $ getFilmFilmo ipool
  logInfo "Fetching Filmo_Realisation table..."
  filmoRealisation <- liftIO $ getFilmoRealisation ipool
  logInfo "Fetching Filmo_LienWikidata table..."
  filmoLienWikidata <- liftIO $ getFilmoLienWikidata ipool
  logInfo "Fetching Nom_LienWikidata table..."
  nomLienWikidata <- liftIO $ getNomLienWikidata ipool
  logInfo "Fetching Filmo_Pays table..."
  filmoPays <- liftIO $ getFilmoPays ipool
  logInfo "Fetching Pays_LienWikidata table..."
  paysLienWikidata <- liftIO $ getPaysLienWikidata ipool
  logInfo "Fetching Filmo_GenresCategories table..."
  filmoGenresCategories <- liftIO $ getFilmoGenresCategories ipool
  logInfo "Fetching GenresCategories table..."
  genresCategoriesLienWikidata <- liftIO $ getGenresCategoriesLienWikidata ipool
  -- logInfo "Fetching Fonction_LienWikidata table..."
  -- fonctionLienWikidata <- liftIO $ getFonctionLienWikidata ipool
  logInfo "Fetching Filmo_Generique table..."
  filmoGenerique <- liftIO $ getFilmoGenerique ipool
  logInfo "Fetching Filmo_Resumes table..."
  filmoResumeEntities <- liftIO $ getFilmoResumes ipool
  logInfo "Fetching Filmo_ResumesAnglais table..."
  filmoResumeAnglaisEntities <- liftIO $ getFilmoResumesAnglais ipool
  logInfo "Fetching Filmo_Langue table..."
  filmoLangue <- liftIO $ getFilmoLangue ipool
  logInfo "Fetching Langue_LienWikidata table..."
  langueLienWikidata <- liftIO $ getLangueLienWikidata ipool
  logInfo "Fetching TypeTitre table..."
  typeTitre <- liftIO $ getTypeTitre ipool
  logInfo "Fetching FilmoTitres table..."
  filmoTitres <- liftIO $ getFilmoTitres ipool
  logInfo "Fetching FilmoDureesOriginales table..."
  filmoDureesOriginales <- liftIO $ getFilmoDureesOriginales ipool

  flip liftSqlPersistMPool opool $ do
    liftIO $ Text.putStrLn "Writing Film entities to database..."
    mapM_ (\c -> insertKey (entityKey c) (entityVal c)) film
    liftIO $ Text.putStrLn "Writing Filmo entities to database..."
    mapM_ (\c -> insertKey (entityKey c) (entityVal c)) filmo
    liftIO $ Text.putStrLn "Writing Nom entities to database..."
    mapM_ (\c -> insertKey (entityKey c) (entityVal c)) nom
    liftIO $ Text.putStrLn "Writing Sujet entities to database..."
    mapM_ (\c -> insertKey (entityKey c) (entityVal c)) sujet
    liftIO $ Text.putStrLn "Writing Pays entities to database..."
    mapM_ (\c -> insertKey (entityKey c) (entityVal c)) pays
    liftIO $ Text.putStrLn "Writing Fonction entities to database..."
    mapM_ (\c -> insertKey (entityKey c) (entityVal c)) fonction
    liftIO $ Text.putStrLn "Writing Langue entities to database..."
    mapM_ (\c -> insertKey (entityKey c) (entityVal c)) langue
    liftIO $ Text.putStrLn "Writing Film_Filmo entities to database..."
    mapM_ (\c -> insertKey (entityKey c) (entityVal c)) filmFilmo
    liftIO $ Text.putStrLn "Writing Film_Realisation entities to database..."
    mapM_ (\c -> insertKey (entityKey c) (entityVal c)) filmoRealisation
    liftIO $ Text.putStrLn "Writing Filmo_LienWikidata entities to database..."
    mapM_ (\c -> insertKey (entityKey c) (entityVal c)) filmoLienWikidata
    liftIO $ Text.putStrLn "Writing Nom_LienWikidata entities to database..."
    mapM_ (\c -> insertKey (entityKey c) (entityVal c)) nomLienWikidata
    liftIO $ Text.putStrLn "Writing Filmo_Pays entities to database..."
    mapM_ (\c -> insertKey (entityKey c) (entityVal c)) filmoPays
    liftIO $ Text.putStrLn "Writing Pays_LienWikidata entities to database..."
    mapM_ (\c -> insertKey (entityKey c) (entityVal c)) paysLienWikidata
    liftIO $ Text.putStrLn "Writing Filmo_GenresCategories entities to database..."
    mapM_ (\c -> insertKey (entityKey c) (entityVal c)) filmoGenresCategories
    liftIO $ Text.putStrLn "Writing GenresCategories_LienWikidata entities to database..."
    mapM_ (\c -> insertKey (entityKey c) (entityVal c)) genresCategoriesLienWikidata
    -- liftIO $ Text.putStrLn "Writing Fonction_LienWikidata entities to database..."
    -- mapM_ (\c -> insertKey (entityKey c) (entityVal c)) fonctionLienWikidata
    liftIO $ Text.putStrLn "Writing Filmo_Generique entities to database..."
    mapM_ (\c -> insertKey (entityKey c) (entityVal c)) filmoGenerique
    liftIO $ Text.putStrLn "Writing Filmo_Resumes entities to database..."
    mapM_ (\c -> insertKey (entityKey c) (entityVal c)) filmoResumeEntities
    liftIO $ Text.putStrLn "Writing Filmo_ResumesAnglais entities to database..."
    mapM_ (\c -> insertKey (entityKey c) (entityVal c)) filmoResumeAnglaisEntities
    liftIO $ Text.putStrLn "Writing Filmo_Langue entities to database..."
    mapM_ (\c -> insertKey (entityKey c) (entityVal c)) filmoLangue
    liftIO $ Text.putStrLn "Writing Langue_LienWikidata entities to database..."
    mapM_ (\c -> insertKey (entityKey c) (entityVal c)) langueLienWikidata
    liftIO $ Text.putStrLn "Writing TypeTitre entities to database..."
    mapM_ (\c -> insertKey (entityKey c) (entityVal c)) typeTitre
    liftIO $ Text.putStrLn "Writing FilmoTitres entities to database..."
    mapM_ (\c -> insertKey (entityKey c) (entityVal c)) filmoTitres
    liftIO $ Text.putStrLn "Writing FilmoDureesOriginales entities to database..."
    mapM_ (\c -> insertKey (entityKey c) (entityVal c)) filmoDureesOriginales

    -- runConduit $ selectSource (distinct $ from $ \film_filmo -> return film_filmo)
    --           .| (CL.mapM_ (\(e :: Entity Pays) -> insertKey (entityKey e) (entityVal e)))
              -- .| (savePays opool)
    -- runConduit $ selectSource (distinct $ from $ \filmo -> return filmo) .| (printT opool)
    -- runConduit $ selectSource (distinct $ from $ \filmo_lienwikidata -> return filmo_lienwikidata) .| (printT opool)
    -- runConduit $ selectSource (distinct $ from $ \filmo_realisation -> return filmo_realisation) .| (printT opool)
    -- runConduit $ selectSource (distinct $ from $ \nom -> return nom) .| (printT opool)
    -- runConduit $ selectSource (distinct $ from $ \nom_lienwikidata -> return nom_lienwikidata) .| (printT opool)
    -- runConduit $ selectSource (distinct $ from $ \filmo_pays -> return filmo_pays) .| (printT opool)
    -- runConduit $ selectSource (distinct $ from $ \pays -> return pays) .| (printT opool)
    -- runConduit $ selectSource (distinct $ from $ \pays_lienwikidata -> return pays_lienwikidata) .| (printT opool)
    -- runConduit $ selectSource (distinct $ from $ \filmo_genrescategories -> return filmo_genrescategories) .| (printT opool)
    -- runConduit $ selectSource (distinct $ from $ \sujet -> return sujet) .| (printT opool)
    -- runConduit $ selectSource (distinct $ from $ \filmo_generique -> return filmo_generique) .| (printT opool)
    -- runConduit $ selectSource (distinct $ from $ \fonction -> return fonction) .| (printT opool)
    -- runConduit $ selectSource (distinct $ from $ \fonction_lienwikidata -> return fonction_lienwikidata) .| (printT opool)

    -- let source = selectSource (distinct $ from $ \pays -> return pays)
    -- liftIO $ flip runSqlPersistMPool opool $ do
    --   runMigration migrateAll
    --   runConduit $ source .| CL.mapM_ (\e -> insertKey (entityKey e) (entityVal e))

-- printT :: (Monad m, MonadIO m)
--        => Pool SqlBackend
--        -> ConduitT Pays Void m ()
-- printT pool = CL.mapM_ (liftIO . Text.putStrLn . paysTerme)

-- savePays :: (Monad m, MonadIO m)
--          => Pool SqlBackend
--          -> ConduitT (Entity Pays) Void m ()
-- savePays pool = CL.mapM_ (\e -> liftIO $ flip runSqlPersistMPool pool $ insertKey (entityKey e) (entityVal e))
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

getFilm :: Pool SqlBackend
        -> IO [Entity Film]
getFilm pool = do
  liftIO $ flip runSqlPersistMPool pool $ do
    select $ distinct $ from $ \film -> do return film

getFilmFilmo :: Pool SqlBackend
             -> IO [Entity Film_Filmo]
getFilmFilmo pool = do
  liftIO $ flip runSqlPersistMPool pool $ do
    select $
      distinct $
      from $ \(film, filmo, filmFilmo) -> do
      where_ ( filmFilmo ^. Film_FilmoFilmId ==. film ^. FilmId
           &&. filmFilmo ^. Film_FilmoFilmoId ==. filmo ^. FilmoId
             )
      return filmFilmo

getNom :: Pool SqlBackend
       -> IO [Entity Nom]
getNom pool = do
  liftIO $ flip runSqlPersistMPool pool $ do
    select $ distinct $ from $ \nom -> return nom

getPays :: Pool SqlBackend
        -> IO [Entity Pays]
getPays pool = do
  liftIO $ flip runSqlPersistMPool pool $ do
    select $ distinct $ from $ \pays -> return pays

getLangue :: Pool SqlBackend
           -> IO [Entity Langue]
getLangue pool = do
  liftIO $ flip runSqlPersistMPool pool $ do
    select $ distinct $ from $ \langues -> return langues

getSujet :: Pool SqlBackend
         -> IO [Entity Sujet]
getSujet pool = do
  liftIO $ flip runSqlPersistMPool pool $ do
    select $ distinct $ from $ \sujet -> do return sujet

getFilmo :: Pool SqlBackend
         -> IO [Entity Filmo]
getFilmo pool = do
  liftIO $ flip runSqlPersistMPool pool $ do
    select $ distinct $ from $ \filmo -> do return filmo

getFilmoLienWikidata :: Pool SqlBackend
                     -> IO [Entity Filmo_LienWikidata]
getFilmoLienWikidata pool = do
  liftIO $ flip runSqlPersistMPool pool $ do
    select $
      distinct $
      from $ \(filmo, filmoLienWikidata) -> do
      where_ $ filmoLienWikidata ^. Filmo_LienWikidataFilmoId ==. filmo ^. FilmoId
      return filmoLienWikidata

getFilmoRealisation :: Pool SqlBackend
                    -> IO [Entity Filmo_Realisation]
getFilmoRealisation pool = do
  liftIO $ flip runSqlPersistMPool pool $ do
    select $
      distinct $
      from $ \(filmo, filmoRealisation, nom) -> do
      where_ ( filmoRealisation ^. Filmo_RealisationFilmoId ==. filmo ^. FilmoId
           &&. filmoRealisation ^. Filmo_RealisationNomId ==. nom ^. NomId
             )
      return filmoRealisation

getNomLienWikidata :: Pool SqlBackend
                   -> IO [Entity Nom_LienWikidata]
getNomLienWikidata pool = do
  liftIO $ flip runSqlPersistMPool pool $ do
    select $
      distinct $
      from $ \(nom, nomLienWikidata) -> do
      where_ $ nomLienWikidata ^. Nom_LienWikidataNomId ==. nom ^. NomId
      return nomLienWikidata

getFilmoPays :: Pool SqlBackend
             -> IO [Entity Filmo_Pays]
getFilmoPays pool = do
  liftIO $ flip runSqlPersistMPool pool $ do
    select $
      distinct $
      from $ \(filmo, pays, filmoPays) -> do
      where_ ( filmoPays ^. Filmo_PaysFilmoId ==. filmo ^. FilmoId
           &&. filmoPays ^. Filmo_PaysPaysId ==. pays ^. PaysId
             )
      return filmoPays

getPaysLienWikidata :: Pool SqlBackend
                    -> IO [Entity Pays_LienWikidata]
getPaysLienWikidata pool = do
  liftIO $ flip runSqlPersistMPool pool $ do
    select $
      distinct $
      from $ \(pays, paysLienWikidata) -> do
      where_ $ paysLienWikidata ^. Pays_LienWikidataPaysId ==. pays ^. PaysId
      return paysLienWikidata

getFilmoGenresCategories :: Pool SqlBackend
                         -> IO [Entity Filmo_GenresCategories]
getFilmoGenresCategories pool = do
  liftIO $ flip runSqlPersistMPool pool $ do
    select $
      distinct $
      from $ \(filmo, sujet, filmoGenresCategories) -> do
      where_ ( filmoGenresCategories ^. Filmo_GenresCategoriesFilmoId ==. filmo ^. FilmoId
           &&. filmoGenresCategories ^. Filmo_GenresCategoriesSujetId ==. sujet ^. SujetId
             )
      return filmoGenresCategories

getGenresCategoriesLienWikidata :: Pool SqlBackend
                                -> IO [Entity GenresCategories_LienWikidata]
getGenresCategoriesLienWikidata pool = do
  liftIO $ flip runSqlPersistMPool pool $ do
    select $
      distinct $
      from $ \(sujet, genresCategoriesLienWikidata) -> do
      where_ ( genresCategoriesLienWikidata ^. GenresCategories_LienWikidataSujetId ==. sujet ^. SujetId
             )
      return genresCategoriesLienWikidata

getFilmoGenerique :: Pool SqlBackend
                  -> IO [Entity Filmo_Generique]
getFilmoGenerique pool = do
  liftIO $ flip runSqlPersistMPool pool $ do
    select $
      distinct $
      from $ \(fonction, filmo, filmoGenerique) -> do
      where_ ( filmoGenerique ^. Filmo_GeneriqueFonctionId ==. fonction ^. FonctionId
           &&. filmoGenerique ^. Filmo_GeneriqueFilmoId ==. filmo ^. FilmoId
           -- &&. filmoGenerique ^. Filmo_GeneriqueOrganismeId ==. sujet ?. SujetId
           -- &&. filmoGenerique ^. Filmo_GeneriqueNomId ==. nom ?. NomId
             )
      return filmoGenerique

getFonction :: Pool SqlBackend
            -> IO [Entity Fonction]
getFonction pool = do
  liftIO $ flip runSqlPersistMPool pool $ do
    select $ distinct $ from $ \fonction -> do return fonction

-- getFonctionLienWikidata :: Pool SqlBackend
--                         -> IO [Entity Fonction_LienWikidata]
-- getFonctionLienWikidata pool = do
--   liftIO $ flip runSqlPersistMPool pool $ do
--     select $
--       distinct $
--       from $ \(fonction, fonctionLienWikidata) -> do
--       where_ $ fonctionLienWikidata ^. Fonction_LienWikidataFonctionId ==. fonction ^. FonctionId
--       return fonctionLienWikidata

getFilmoResumes :: Pool SqlBackend
                -> IO [Entity FilmoResumes]
getFilmoResumes pool = do
  liftIO $ flip runSqlPersistMPool pool $ do
    select $
      distinct $
      from $ \(filmo, filmoResumes) -> do
      where_ ( filmoResumes ^. FilmoResumesFilmoId ==. filmo ^. FilmoId
             )
      return filmoResumes

getFilmoResumesAnglais :: Pool SqlBackend
                       -> IO [Entity FilmoResumesAnglais]
getFilmoResumesAnglais pool = do
  liftIO $ flip runSqlPersistMPool pool $ do
    select $
      distinct $
      from $ \(filmo, filmoResumes) -> do
      where_ ( filmoResumes ^. FilmoResumesAnglaisFilmoId ==. filmo ^. FilmoId
             )
      return filmoResumes

getFilmoLangue :: Pool SqlBackend
               -> IO [Entity Filmo_Langue]
getFilmoLangue pool = do
  liftIO $ flip runSqlPersistMPool pool $ do
    select $
      distinct $
      from $ \(filmo, langue, filmoLangue) -> do
      where_ ( filmoLangue ^. Filmo_LangueFilmoId ==. filmo ^. FilmoId
           &&. filmoLangue ^. Filmo_LangueLangueId ==. langue ^. LangueId
             )
      return filmoLangue

getLangueLienWikidata :: (MonadIO m)
                      => Pool SqlBackend
                      -> m [Entity Langue_LienWikidata]
getLangueLienWikidata pool = do
  liftIO $ flip liftSqlPersistMPool pool $ do
    select $
      distinct $
      from $ \(langue, langueLienWikidata) -> do
      where_ $ langueLienWikidata ^. Langue_LienWikidataLangueId ==. langue ^. LangueId
      return langueLienWikidata

getTypeTitre :: (MonadIO m)
             => Pool SqlBackend
             -> m [Entity TypeTitre]
getTypeTitre pool = do
  liftIO $ flip liftSqlPersistMPool pool $ do
    select $ distinct $ from $ return

getFilmoTitres :: (MonadIO m)
               => Pool SqlBackend
               -> m [Entity FilmoTitres]
getFilmoTitres pool = do
  liftIO $ flip liftSqlPersistMPool pool $ do
    select $
      distinct $
      from $ \(filmo, filmoTitres) -> do
      where_ $ filmoTitres ^. FilmoTitresFilmoId ==. filmo ^. FilmoId
      return filmoTitres

getFilmoDureesOriginales :: (MonadIO m)
                         => Pool SqlBackend
                         -> m [Entity FilmoDureesOriginales]
getFilmoDureesOriginales pool = do
  liftIO $ flip liftSqlPersistMPool pool $ do
    select $
      distinct $
      from $ \(filmo, filmoDureesOriginales) -> do
      where_ $ filmoDureesOriginales ^. FilmoDureesOriginalesFilmoId ==. filmo ^. FilmoId
      return filmoDureesOriginales

