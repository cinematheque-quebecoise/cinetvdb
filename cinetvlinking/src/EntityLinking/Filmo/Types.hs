{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module EntityLinking.Filmo.Types
  ( FilmoId (..),
    FilmoFeatures (..),
  )
where

import Data.Csv (DefaultOrdered (..), FromField (parseField), FromNamedRecord (..), Parser, ToField (toField), ToNamedRecord (..), header, namedRecord, (.:), (.=))
import Import

newtype FilmoId = FilmoId {unFilmoId :: Int64}
  deriving (Eq, Ord, Show)

instance FromField FilmoId where
  parseField pid = FilmoId <$> (parseField pid :: Parser Int64)

instance ToField FilmoId where
  toField (FilmoId pid) = toField pid

data FilmoFeatures = FilmoFeatures
  { filmoId :: FilmoId,
    title :: Text,
    year :: Maybe Int,
    directors :: Text
  }
  deriving (Eq, Show)

instance FromNamedRecord FilmoFeatures where
  parseNamedRecord r =
    FilmoFeatures
      <$> r .: "FilmoID"
      <*> r .: "Titre"
      <*> r .: "Annee"
      <*> r .: "Realisateurs"

instance DefaultOrdered FilmoFeatures where
  headerOrder _ = header ["FilmoID", "Titre", "Annee", "Realisateurs"]

instance ToNamedRecord FilmoFeatures where
  toNamedRecord FilmoFeatures {..} =
    namedRecord
      [ "FilmoID" .= filmoId,
        "Titre" .= title,
        "Annee" .= year,
        "Realisateurs" .= directors
      ]
