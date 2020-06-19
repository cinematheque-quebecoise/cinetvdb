{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Types.PersonFeatures where

import Import

import Data.Csv (namedRecord, DefaultOrdered(..), header, ToNamedRecord(..), FromNamedRecord(..), (.:), (.=))

data PersonFeatures = PersonFeatures
  { personId :: Int64
  , firstname :: Text
  , lastname :: Text
  , notes :: Text
  , fonctions :: Text
  } deriving (Show)

instance FromNamedRecord PersonFeatures where
  parseNamedRecord r = PersonFeatures
                   <$> r .: "NomID"
                   <*> r .: "Prenom"
                   <*> r .: "Nom"
                   <*> r .: "Notes"
                   <*> r .: "Fonctions"

instance DefaultOrdered PersonFeatures where
  headerOrder _ = header ["NomID", "Prenom", "Nom", "Notes", "Fonctions"]

instance ToNamedRecord PersonFeatures where
  toNamedRecord PersonFeatures{..} =
    namedRecord
      [ "NomID" .= personId
      , "Prenom" .= firstname
      , "Nom" .= lastname
      , "Notes" .= notes
      , "Fonctions" .= fonctions
      ]

-- | Read CSV data from file
-- readPeopleData :: FilePath -> RIO App (Either String (Header, Records PersonFeatures))
-- readPeopleData fpath = do
--   csvData <- liftIO $ BL.readFile fpath
--   return $ decodeByName csvData



