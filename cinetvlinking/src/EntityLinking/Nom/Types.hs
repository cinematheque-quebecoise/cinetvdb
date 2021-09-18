{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module EntityLinking.Nom.Types
  ( PersonId (..),
    PersonFeatures (..),
  )
where

import Data.Csv (DefaultOrdered (..), FromField (parseField), FromNamedRecord (..), Parser, ToField (toField), ToNamedRecord (..), header, namedRecord, (.:), (.=))
import Import

newtype PersonId = PersonId {unPersonId :: Int64}
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

instance FromField PersonId where
  parseField pid = PersonId <$> (parseField pid :: Parser Int64)

instance ToField PersonId where
  toField (PersonId pid) = toField pid

data PersonFeatures = PersonFeatures
  { personId :: PersonId,
    firstname :: Text,
    lastname :: Text,
    notes :: Text,
    fonctions :: Text
  }
  deriving (Eq, Show)

instance FromNamedRecord PersonFeatures where
  parseNamedRecord r =
    PersonFeatures
      <$> r .: "NomID"
      <*> r .: "Prenom"
      <*> r .: "Nom"
      <*> r .: "Notes"
      <*> r .: "Fonctions"

instance DefaultOrdered PersonFeatures where
  headerOrder _ = header ["NomID", "Prenom", "Nom", "Notes", "Fonctions"]

instance ToNamedRecord PersonFeatures where
  toNamedRecord PersonFeatures {..} =
    namedRecord
      [ "NomID" .= personId,
        "Prenom" .= firstname,
        "Nom" .= lastname,
        "Notes" .= notes,
        "Fonctions" .= fonctions
      ]
