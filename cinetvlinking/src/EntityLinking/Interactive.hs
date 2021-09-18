{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module EntityLinking.Interactive
  ( interact,
    promptNomId,
    promptFilmoId,
  )
where

import Data.Either.Combinators (rightToMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import EntityLinking.Wikidata (MonadWikidataLinking, getWikidataLink)
import Import
import EntityLinking.Filmo.Types (FilmoFeatures)
import EntityLinking.Nom.Types (PersonFeatures)
import EntityLinking.Types (ResourceUri (..))

newtype ReadException = ReadException Text deriving (Typeable)

instance Show ReadException where
  show (ReadException s) =
    concat
      ["L'identifiant ", show s, " est invalide. Il doit être un entier positif."]

instance Exception ReadException

interact :: RIO App () -> RIO App ()
interact promptEntity = do
  liftIO $ T.putStrLn "Bienvenue dans l'interface interactive du projet!"
  liftIO $
    T.putStrLn
      "L'interface permet de tester l'algorithme pour lier les entités à Wikidata.\n"

  forever $ catch promptEntity showE
  where
    showE (SomeException e) = liftIO $ T.putStrLn $ T.pack $ show e <> "\n"

promptNomId :: RIO App ()
promptNomId = do
  inputNomId <- liftIO $ prompt "Entrez l'identifiant d'une personne: "
  promptEntityId (Proxy @PersonFeatures) inputNomId

promptFilmoId :: RIO App ()
promptFilmoId = do
  inputFilmoId <- liftIO $ prompt "Entrez l'identifiant d'une oeuvre: "
  promptEntityId (Proxy @FilmoFeatures) inputFilmoId

promptEntityId ::
  ( MonadWikidataLinking (RIO env) a
  ) =>
  Proxy a ->
  Text ->
  RIO env ()
promptEntityId p inputEntityId = do
  entityId <- readEntityId inputEntityId

  linkedEntityE <- getWikidataLink p entityId

  case linkedEntityE of
    Right linkedEntity ->
      liftIO $
        T.putStrLn $
          "L'identifiant "
            <> T.pack (show entityId)
            <> " a été lié à l'entité "
            <> unResourceUri linkedEntity
            <> "\n"
    Left e -> do
      liftIO $ T.putStrLn "Cette entité n'a pas pu être lié à Wikidata\n"
      throwM e

readEntityId :: (MonadThrow m, Integral n) => Text -> m n
readEntityId inputNomId = case fmap fst $ rightToMaybe $ T.decimal inputNomId of
  Just nomId -> return nomId
  Nothing -> throwM $ ReadException inputNomId

prompt :: Text -> IO Text
prompt text = do
  T.putStr text
  hFlush stdout
  T.getLine
