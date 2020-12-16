{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Run.NomLinking.Interactive
  ( interact
  )
where

import           Import

import           Data.Either.Combinators    (rightToMaybe)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import qualified Data.Text.Read             as T
import           EntityLinking.Nom.Wikidata (LinkedPerson(..), linkNomId)
import           Types.Resource             (ResourceUri (..))

newtype ReadException = ReadException Text deriving (Typeable)

instance Show ReadException where
  show (ReadException s) = concat
    ["L'identifiant ", show s, " est invalide. Il doit être un entier positif."]

instance Exception ReadException

interact :: RIO App ()
interact = do
  liftIO $ T.putStrLn "Bienvenue dans l'interface interactive du projet!"
  liftIO
    $ T.putStrLn
        "L'interface permet de tester l'algorithme pour lier les personnes à Wikidata.\n"

  forever $ catch promptNomId showE
  where showE (SomeException e) = liftIO $ T.putStrLn $ T.pack $ show e <> "\n"

promptNomId :: RIO App ()
promptNomId = do
  inputNomId      <- liftIO $ prompt "Entrez l'identifiant d'une personne: "

  nomId           <- readNomId inputNomId

  linkedPersonE <- linkNomId nomId

  case linkedPersonE of
    Right linkedPerson ->
      liftIO
        $  T.putStrLn
        $  T.pack (show $ linkedPersonName linkedPerson)
        <> " a été lié à l'entité "
        <> unResourceUri (linkedPersonUri linkedPerson)
        <> "\n"
    Left e -> do
      liftIO $ T.putStrLn "Cette personne n'a pas pu être lié à Wikidata\n"
      throwM e

readNomId :: (MonadThrow m, Integral n) => Text -> m n
readNomId inputNomId = case fmap fst $ rightToMaybe $ T.decimal inputNomId of
  Just nomId -> return nomId
  Nothing    -> throwM $ ReadException inputNomId

prompt :: Text -> IO Text
prompt text = do
  T.putStr text
  hFlush stdout
  T.getLine
