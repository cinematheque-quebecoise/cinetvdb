{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import           Control.Monad.IO.Class       (MonadIO (..))
import           Data.Pool                    (Pool)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.List as L
import qualified Data.Text.IO                 as T
import           Database.CineTv.Public.Model
import           Database.Esqueleto           hiding (get)
import           Database.Persist.Sqlite      (SqliteConf (..))
import           System.Console.Docopt
import           System.Directory
import           System.Environment           (getArgs)
import           System.Exit                  (exitFailure)
import           System.FilePath              (joinPath)
import           Text.RE.TDFA.Text
import           UnliftIO.Exception           (tryIO)
import Data.Maybe (catMaybes)

patterns :: Docopt
patterns = [docopt|
cinetv2public version 0.1.0

Usage:
  cinetv2public -s=<sqlitedbpath> -d=<outputdir>

Options:
  -h --help    show this
  -s <sqlitedbpath>    Path of SQLite db path
  -d <outputdir>    Directory of output file
|]

getArgOrExit :: Arguments -> Option -> IO String
getArgOrExit = getArgOrExitWith patterns

main :: IO ()
main = do
  args         <- parseArgsOrExit patterns =<< getArgs

  sqliteDbPath <- args `getArgOrExit` shortOption 's'
  outputDir    <- args `getArgOrExit` shortOption 'd'

  runCineTvMigration (T.pack sqliteDbPath) (T.pack outputDir)

runCineTvMigration
  :: Text -- ^ CineTV SQLite database path
  -> Text -- ^ New SQLite database path
  -> IO ()
runCineTvMigration sqliteDbPath outputDir =
  case matchedText $ sqliteDbPath ?=~ [re|[0-9]+-[0-9]+-[0-9]+|] of
    Just date -> do
      let newSqliteDbFname = "cinetv-" <> date <> "-publique.db"
      let newSqliteDbPath = (T.pack . joinPath . fmap T.unpack)
            [outputDir, newSqliteDbFname]

      removeOldDatabase newSqliteDbPath

      T.putStrLn
        $  "Migrating SQLite database "
        <> sqliteDbPath
        <> " to "
        <> newSqliteDbPath
      migrateDatabase sqliteDbPath newSqliteDbPath

    Nothing -> do
      putStrLn "The SQLite file must contain a date in the format YYYY-MM-DD"
      exitFailure

removeOldDatabase
  :: (MonadIO m)
  => Text -- ^ New SQLite database path
  -> m ()
removeOldDatabase newSqliteDbPath = do
  _ <- liftIO $ tryIO $ removeFile $ T.unpack newSqliteDbPath
  return ()

migrateDatabase
  :: (MonadIO m)
  => Text -- ^ CineTV SQLite database path
  -> Text -- ^ New SQLite database path
  -> m ()
migrateDatabase sqliteDbPath newSqliteDbPath = do
  ipool <- liftIO $ createPoolConfig (SqliteConf sqliteDbPath 1)
  opool <- liftIO $ createPoolConfig (SqliteConf newSqliteDbPath 1)

  liftIO $ flip runSqlPersistMPool opool $ runMigration migrateAll

  -- countries <- liftIO $ getCountries ipool
  -- liftIO $ flip runSqlPersistMPool opool $ do
  --   mapM_ (\c -> insertKey (entityKey c) (entityVal c)) countries

  -- putStrLn "Migrating Film_Filmo table..."
  -- liftIO $ putStrLn "Fetching Film table..."
  -- film <- liftIO $ getFilm ipool
  liftIO $ putStrLn "Fetching Filmo table..."
  filmo <- liftIO $ getFilmo ipool
  liftIO $ putStrLn "Fetching Nom table..."
  nom <- liftIO $ getNom ipool
  liftIO $ putStrLn "Fetching Sujet table..."
  sujet <- liftIO $ getSujet ipool
  liftIO $ putStrLn "Fetching Pays table..."
  pays <- liftIO $ getPays ipool
  liftIO $ putStrLn "Fetching Langue table..."
  langue <- liftIO $ getLangue ipool
  liftIO $ putStrLn "Fetching Fonction table..."
  fonction <- liftIO $ getFonction ipool
  -- liftIO $ putStrLn "Fetching Film_Filmo table..."
  -- filmFilmo <- liftIO $ getFilmFilmo ipool
  liftIO $ putStrLn "Fetching Filmo_Realisation table..."
  filmoRealisation <- liftIO $ getFilmoRealisation ipool
  liftIO $ putStrLn "Fetching Filmo_LienWikidata table..."
  filmoLienWikidata <- liftIO $ getFilmoLienWikidata ipool
  liftIO $ putStrLn "Fetching Nom_LienWikidata table..."
  nomLienWikidata <- liftIO $ getNomLienWikidata ipool
  liftIO $ putStrLn "Fetching Filmo_Pays table..."
  filmoPays <- liftIO $ getFilmoPays ipool
  liftIO $ putStrLn "Fetching Pays_LienWikidata table..."
  paysLienWikidata <- liftIO $ getPaysLienWikidata ipool
  liftIO $ putStrLn "Fetching Filmo_GenresCategories table..."
  filmoGenresCategories <- liftIO $ getFilmoGenresCategories ipool
  liftIO $ putStrLn "Fetching GenresCategories table..."
  genresCategoriesLienWikidata <- liftIO $ getGenresCategoriesLienWikidata ipool
  -- liftIO $ putStrLn "Fetching Fonction_LienWikidata table..."
  -- fonctionLienWikidata <- liftIO $ getFonctionLienWikidata ipool
  liftIO $ putStrLn "Fetching Filmo_Generique table..."
  filmoGenerique <- liftIO $ getFilmoGenerique ipool
  liftIO $ putStrLn "Fetching Filmo_Resumes table..."
  filmoResumeEntities <- liftIO $ getFilmoResumes ipool
  liftIO $ putStrLn "Fetching Filmo_ResumesAnglais table..."
  filmoResumeAnglaisEntities <- liftIO $ getFilmoResumesAnglais ipool
  liftIO $ putStrLn "Fetching Filmo_Langue table..."
  filmoLangue <- liftIO $ getFilmoLangue ipool
  liftIO $ putStrLn "Fetching Langue_LienWikidata table..."
  langueLienWikidata <- liftIO $ getLangueLienWikidata ipool
  liftIO $ putStrLn "Fetching TypeTitre table..."
  typeTitre <- liftIO $ getTypeTitre ipool
  liftIO $ putStrLn "Fetching FilmoTitres table..."
  filmoTitres <- liftIO $ getFilmoTitres ipool
  liftIO $ putStrLn "Fetching FilmoDureesOriginales table..."
  filmoDureesOriginales <- liftIO $ getFilmoDureesOriginales ipool
  liftIO $ putStrLn "Fetching NatureDeLaProduction table..."
  natureDeLaProduction <- liftIO $ getNatureDeLaProduction ipool

  flip liftSqlPersistMPool opool $ do
    -- liftIO $ T.putStrLn "Writing Film entities to database..."
    -- mapM_ (\c -> insertKey (entityKey c) (entityVal c)) film
    liftIO $ T.putStrLn "Writing NatureDeLaProduction entities to database..."
    mapM_ (\c -> insertKey (entityKey c) (entityVal c)) natureDeLaProduction
    liftIO $ T.putStrLn "Writing Filmo entities to database..."
    mapM_ (\c -> insertKey (entityKey c) (entityVal c)) filmo
    liftIO $ T.putStrLn "Writing Nom entities to database..."
    mapM_ (\c -> insertKey (entityKey c) (entityVal c)) nom
    liftIO $ T.putStrLn "Writing Sujet entities to database..."
    mapM_ (\c -> insertKey (entityKey c) (entityVal c)) sujet
    liftIO $ T.putStrLn "Writing Pays entities to database..."
    mapM_ (\c -> insertKey (entityKey c) (entityVal c)) pays
    liftIO $ T.putStrLn "Writing Fonction entities to database..."
    mapM_ (\c -> insertKey (entityKey c) (entityVal c)) fonction
    liftIO $ T.putStrLn "Writing Langue entities to database..."
    mapM_ (\c -> insertKey (entityKey c) (entityVal c)) langue
    -- liftIO $ T.putStrLn "Writing Film_Filmo entities to database..."
    -- mapM_ (\c -> insertKey (entityKey c) (entityVal c)) filmFilmo
    liftIO $ T.putStrLn "Writing Filmo_Realisation entities to database..."
    mapM_ (\c -> insertKey (entityKey c) (entityVal c)) filmoRealisation
    liftIO $ T.putStrLn "Writing Filmo_LienWikidata entities to database..."
    mapM_ (\c -> insertKey (entityKey c) (entityVal c)) filmoLienWikidata
    liftIO $ T.putStrLn "Writing Nom_LienWikidata entities to database..."
    mapM_ (\c -> insertKey (entityKey c) (entityVal c)) nomLienWikidata
    liftIO $ T.putStrLn "Writing Filmo_Pays entities to database..."
    mapM_ (\c -> insertKey (entityKey c) (entityVal c)) filmoPays
    liftIO $ T.putStrLn "Writing Pays_LienWikidata entities to database..."
    mapM_ (\c -> insertKey (entityKey c) (entityVal c)) paysLienWikidata
    liftIO
      $ T.putStrLn "Writing Filmo_GenresCategories entities to database..."
    mapM_ (\c -> insertKey (entityKey c) (entityVal c)) filmoGenresCategories
    liftIO $ T.putStrLn
      "Writing GenresCategories_LienWikidata entities to database..."
    mapM_ (\c -> insertKey (entityKey c) (entityVal c))
          genresCategoriesLienWikidata
    -- liftIO $ T.putStrLn "Writing Fonction_LienWikidata entities to database..."
    -- mapM_ (\c -> insertKey (entityKey c) (entityVal c)) fonctionLienWikidata
    liftIO $ T.putStrLn "Writing Filmo_Generique entities to database..."
    mapM_ (\c -> insertKey (entityKey c) (entityVal c)) filmoGenerique
    liftIO $ T.putStrLn "Writing Filmo_Resumes entities to database..."
    mapM_ (\c -> insertKey (entityKey c) (entityVal c)) filmoResumeEntities
    liftIO
      $ T.putStrLn "Writing Filmo_ResumesAnglais entities to database..."
    mapM_ (\c -> insertKey (entityKey c) (entityVal c))
          filmoResumeAnglaisEntities
    liftIO $ T.putStrLn "Writing Filmo_Langue entities to database..."
    mapM_ (\c -> insertKey (entityKey c) (entityVal c)) filmoLangue
    liftIO $ T.putStrLn "Writing Langue_LienWikidata entities to database..."
    mapM_ (\c -> insertKey (entityKey c) (entityVal c)) langueLienWikidata
    liftIO $ T.putStrLn "Writing TypeTitre entities to database..."
    mapM_ (\c -> insertKey (entityKey c) (entityVal c)) typeTitre
    liftIO $ T.putStrLn "Writing FilmoTitres entities to database..."
    mapM_ (\c -> insertKey (entityKey c) (entityVal c)) filmoTitres
    liftIO
      $ T.putStrLn "Writing FilmoDureesOriginales entities to database..."
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
-- printT pool = CL.mapM_ (liftIO . T.putStrLn . paysTerme)

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

-- getFilm :: Pool SqlBackend -> IO [Entity Film]
-- getFilm pool =
--   liftIO $ flip runSqlPersistMPool pool $ select $ distinct $ from return

-- getFilmFilmo :: Pool SqlBackend -> IO [Entity Film_Filmo]
-- getFilmFilmo pool =
--   liftIO
--     $ flip runSqlPersistMPool pool
--     $ select
--     $ distinct
--     $ from
--     $ \(film, filmo, filmFilmo) -> do
--         where_
--           (   (filmFilmo ^. Film_FilmoFilmId)
--           ==. (film ^. FilmId)
--           &&. (filmFilmo ^. Film_FilmoFilmoId)
--           ==. (filmo ^. FilmoId)
--           )
--         return filmFilmo

getNom :: Pool SqlBackend -> IO [Entity Nom]
getNom pool = do
  nomEntitiesGenerique <-
    liftIO
      $ flip liftSqlPersistMPool pool
      $ select
      $ distinct
      $ from
      $ \(filmoGenerique, nom) -> do
          where_ (   nom ?.  NomId ==. filmoGenerique ^.  Filmo_GeneriqueNomId)
          return nom
  nomEntitiesRealisation <-
    liftIO
      $ flip liftSqlPersistMPool pool
      $ select
      $ distinct
      $ from
      $ \(filmoRealisation, nom) -> do
          where_ ( nom ?.  NomId ==. filmoRealisation ?.  Filmo_RealisationNomId)
          return nom
  return $ L.nub $ catMaybes (nomEntitiesGenerique ++ nomEntitiesRealisation)

getPays :: Pool SqlBackend -> IO [Entity Pays]
getPays pool =
  liftIO $ flip runSqlPersistMPool pool $ select $ distinct $ from return

getLangue :: Pool SqlBackend -> IO [Entity Langue]
getLangue pool =
  liftIO $ flip runSqlPersistMPool pool $ select $ distinct $ from return

getSujet :: Pool SqlBackend -> IO [Entity Sujet]
getSujet pool =
  liftIO $ flip runSqlPersistMPool pool $ select $ distinct $ from return

getFilmo :: Pool SqlBackend -> IO [Entity Filmo]
getFilmo pool =
  liftIO $ flip runSqlPersistMPool pool $ select $ distinct $ from return

getFilmoLienWikidata :: Pool SqlBackend -> IO [Entity Filmo_LienWikidata]
getFilmoLienWikidata pool =
  liftIO
    $ flip runSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \(filmo, filmoLienWikidata) -> do
        where_
          $   filmoLienWikidata
          ^.  Filmo_LienWikidataFilmoId
          ==. filmo
          ^.  FilmoId
        return filmoLienWikidata

getFilmoRealisation :: Pool SqlBackend -> IO [Entity Filmo_Realisation]
getFilmoRealisation pool =
  liftIO
    $ flip runSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \(filmo, filmoRealisation, nom) -> do
        where_
          (   (filmoRealisation ^. Filmo_RealisationFilmoId)
          ==. (filmo ^. FilmoId)
          &&. (filmoRealisation ^. Filmo_RealisationNomId)
          ==. (nom ^. NomId)
          )
        return filmoRealisation

getNomLienWikidata :: Pool SqlBackend -> IO [Entity Nom_LienWikidata]
getNomLienWikidata pool = do
  nomLienWikidataEntitiesGenerique <-
    liftIO
      $ flip liftSqlPersistMPool pool
      $ select
      $ distinct
      $ from
      $ \(filmoGenerique, nomLienWikidata) -> do
          where_ (   nomLienWikidata ?.  Nom_LienWikidataNomId ==. filmoGenerique ^.  Filmo_GeneriqueNomId)
          return nomLienWikidata
  nomLienWikidataEntitiesRealisation <-
    liftIO
      $ flip liftSqlPersistMPool pool
      $ select
      $ distinct
      $ from
      $ \(filmoRealisation, nomLienWikidata) -> do
          where_ ( nomLienWikidata ?.  Nom_LienWikidataNomId ==. filmoRealisation ?.  Filmo_RealisationNomId)
          return nomLienWikidata

  return $ L.nub $ catMaybes (nomLienWikidataEntitiesGenerique ++ nomLienWikidataEntitiesRealisation)

getFilmoPays :: Pool SqlBackend -> IO [Entity Filmo_Pays]
getFilmoPays pool =
  liftIO
    $ flip runSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \(filmo, pays, filmoPays) -> do
        where_
          (   (filmoPays ^. Filmo_PaysFilmoId)
          ==. (filmo ^. FilmoId)
          &&. (filmoPays ^. Filmo_PaysPaysId)
          ==. (pays ^. PaysId)
          )
        return filmoPays

getPaysLienWikidata :: Pool SqlBackend -> IO [Entity Pays_LienWikidata]
getPaysLienWikidata pool =
  liftIO
    $ flip runSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \(pays, paysLienWikidata) -> do
        where_ $ paysLienWikidata ^. Pays_LienWikidataPaysId ==. pays ^. PaysId
        return paysLienWikidata

getFilmoGenresCategories
  :: Pool SqlBackend -> IO [Entity Filmo_GenresCategories]
getFilmoGenresCategories pool =
  liftIO
    $ flip runSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \(filmo, sujet, filmoGenresCategories) -> do
        where_
          (   (filmoGenresCategories ^. Filmo_GenresCategoriesFilmoId)
          ==. (filmo ^. FilmoId)
          &&. (filmoGenresCategories ^. Filmo_GenresCategoriesSujetId)
          ==. (sujet ^. SujetId)
          )
        return filmoGenresCategories

getGenresCategoriesLienWikidata
  :: Pool SqlBackend -> IO [Entity GenresCategories_LienWikidata]
getGenresCategoriesLienWikidata pool =
  liftIO
    $ flip runSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \(sujet, genresCategoriesLienWikidata) -> do
        where_
          (   genresCategoriesLienWikidata
          ^.  GenresCategories_LienWikidataSujetId
          ==. sujet
          ^.  SujetId
          )
        return genresCategoriesLienWikidata

getFilmoGenerique :: Pool SqlBackend -> IO [Entity Filmo_Generique]
getFilmoGenerique pool =
  liftIO
    $ flip runSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \(fonction, filmo, filmoGenerique) -> do
        where_
          (   (filmoGenerique ^. Filmo_GeneriqueFonctionId)
          ==. (fonction ^. FonctionId)
          &&. (filmoGenerique ^. Filmo_GeneriqueFilmoId)
          ==. (filmo ^. FilmoId)
             -- &&. filmoGenerique ^. Filmo_GeneriqueOrganismeId ==. sujet ?. SujetId
             -- &&. filmoGenerique ^. Filmo_GeneriqueNomId ==. nom ?. NomId
          )
        return filmoGenerique

getFonction :: Pool SqlBackend -> IO [Entity Fonction]
getFonction pool =
  liftIO $ flip runSqlPersistMPool pool $ select $ distinct $ from return

-- getFonctionLienWikidata :: Pool SqlBackend
--                         -> IO [Entity Fonction_LienWikidata]
-- getFonctionLienWikidata pool = do
--   liftIO $ flip runSqlPersistMPool pool $ do
--     select $
--       distinct $
--       from $ \(fonction, fonctionLienWikidata) -> do
--       where_ $ fonctionLienWikidata ^. Fonction_LienWikidataFonctionId ==. fonction ^. FonctionId
--       return fonctionLienWikidata

getFilmoResumes :: Pool SqlBackend -> IO [Entity FilmoResumes]
getFilmoResumes pool =
  liftIO
    $ flip runSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \(filmo, filmoResumes) -> do
        where_ (filmoResumes ^. FilmoResumesFilmoId ==. filmo ^. FilmoId)
        return filmoResumes

getFilmoResumesAnglais :: Pool SqlBackend -> IO [Entity FilmoResumesAnglais]
getFilmoResumesAnglais pool =
  liftIO
    $ flip runSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \(filmo, filmoResumes) -> do
        where_ (filmoResumes ^. FilmoResumesAnglaisFilmoId ==. filmo ^. FilmoId)
        return filmoResumes

getFilmoLangue :: Pool SqlBackend -> IO [Entity Filmo_Langue]
getFilmoLangue pool =
  liftIO
    $ flip runSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \(filmo, langue, filmoLangue) -> do
        where_
          (   (filmoLangue ^. Filmo_LangueFilmoId)
          ==. (filmo ^. FilmoId)
          &&. (filmoLangue ^. Filmo_LangueLangueId)
          ==. (langue ^. LangueId)
          )
        return filmoLangue

getLangueLienWikidata
  :: (MonadIO m) => Pool SqlBackend -> m [Entity Langue_LienWikidata]
getLangueLienWikidata pool =
  liftIO
    $ flip liftSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \(langue, langueLienWikidata) -> do
        where_
          $   langueLienWikidata
          ^.  Langue_LienWikidataLangueId
          ==. langue
          ^.  LangueId
        return langueLienWikidata

getTypeTitre :: (MonadIO m) => Pool SqlBackend -> m [Entity TypeTitre]
getTypeTitre pool =
  liftIO $ flip liftSqlPersistMPool pool $ select $ distinct $ from return

getFilmoTitres :: (MonadIO m) => Pool SqlBackend -> m [Entity FilmoTitres]
getFilmoTitres pool =
  liftIO
    $ flip liftSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \(filmo, filmoTitres) -> do
        where_ $ filmoTitres ^. FilmoTitresFilmoId ==. filmo ^. FilmoId
        return filmoTitres

getFilmoDureesOriginales
  :: (MonadIO m) => Pool SqlBackend -> m [Entity FilmoDureesOriginales]
getFilmoDureesOriginales pool =
  liftIO
    $ flip liftSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \(filmo, filmoDureesOriginales) -> do
        where_
          $   (filmoDureesOriginales ^.  FilmoDureesOriginalesFilmoId)
          ==. (filmo ^.  FilmoId)
        return filmoDureesOriginales

getNatureDeLaProduction :: Pool SqlBackend -> IO [Entity NatureDeLaProduction]
getNatureDeLaProduction pool =
  liftIO $ flip runSqlPersistMPool pool $ select $ distinct $ from return

