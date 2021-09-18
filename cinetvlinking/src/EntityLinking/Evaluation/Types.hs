{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module EntityLinking.Evaluation.Types
  ( AnnotatedFeatures (..)
  , AlgorithmResult(..)
  )
where

import Data.Csv
  ( DefaultOrdered (..),
    FromField (..),
    FromNamedRecord (..),
    ToField (..),
    ToNamedRecord (..),
    ToRecord (..),
    header,
    namedRecord,
    record,
    (.:),
    (.=),
  )
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Read as Text
import qualified Data.Vector as Vector
import EntityLinking.Dataset (DatasetValue)
import EntityLinking.Filmo.Types (FilmoFeatures)
import EntityLinking.Nom.Types
import Import

data AnnotatedFeatures i = AnnotatedFeatures
  { features :: i,
    output :: Text,
    isAnnotated :: Bool
  }
  deriving (Show)

instance FromNamedRecord (AnnotatedFeatures PersonFeatures) where
  parseNamedRecord r =
    AnnotatedFeatures
      <$> parseNamedRecord r
      <*> (r .: "LienWikidata")
      <*> (r .: "Verifie")

instance FromNamedRecord (AnnotatedFeatures FilmoFeatures) where
  parseNamedRecord r =
    AnnotatedFeatures
      <$> parseNamedRecord r
      <*> (r .: "LienWikidata")
      <*> (r .: "Verifie")

instance (ToRecord i) => ToRecord (AnnotatedFeatures i) where
  toRecord AnnotatedFeatures {..} =
    toRecord features <> record [encodeUtf8 output]

instance (ToNamedRecord i) => ToNamedRecord (AnnotatedFeatures i) where
  toNamedRecord AnnotatedFeatures {..} =
    namedRecord $
      HM.toList (toNamedRecord features)
        <> ["LienWikidata" .= output, "Verifie" .= isAnnotated]

instance DefaultOrdered (AnnotatedFeatures PersonFeatures) where
  headerOrder _ =
    headerOrder (undefined :: PersonFeatures)
      <> Vector.fromList ["LienWikidata", "Verifie"]

instance DefaultOrdered (AnnotatedFeatures FilmoFeatures) where
  headerOrder _ =
    headerOrder (undefined :: FilmoFeatures)
      <> Vector.fromList ["LienWikidata", "Verifie"]

instance FromField Bool where
  parseField s =
    let intValMaybe = fst <$> eitherToMaybe (Text.decimal $ Text.decodeUtf8 s)
     in maybe
          (pure False)
          (\i -> if i <= (0 :: Integer) then pure False else pure True)
          intValMaybe

instance ToField Bool where
  toField True = "1"
  toField False = "0"

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe e = case e of
  Left _ -> Nothing
  Right v -> Just v

data AlgorithmResult i = AlgorithmResult
  { input :: i,
    predictedValue :: Maybe Text
  }
  deriving (Show)

instance FromNamedRecord (AlgorithmResult PersonFeatures) where
  parseNamedRecord r =
    AlgorithmResult <$> parseNamedRecord r <*> (r .: "LienWikidata")

instance FromNamedRecord (AlgorithmResult (AnnotatedFeatures PersonFeatures)) where
  parseNamedRecord r =
    AlgorithmResult <$> parseNamedRecord r <*> (r .: "LienWikidataPredicted")

instance FromNamedRecord (AlgorithmResult (DatasetValue PersonFeatures)) where
  parseNamedRecord r =
    AlgorithmResult <$> parseNamedRecord r <*> (r .: "LienWikidataPredicted")

instance FromNamedRecord (AlgorithmResult FilmoFeatures) where
  parseNamedRecord r =
    AlgorithmResult <$> parseNamedRecord r <*> (r .: "LienWikidata")

instance FromNamedRecord (AlgorithmResult (AnnotatedFeatures FilmoFeatures)) where
  parseNamedRecord r =
    AlgorithmResult <$> parseNamedRecord r <*> (r .: "LienWikidataPredicted")

instance FromNamedRecord (AlgorithmResult (DatasetValue FilmoFeatures)) where
  parseNamedRecord r =
    AlgorithmResult <$> parseNamedRecord r <*> (r .: "LienWikidataPredicted")

instance ToNamedRecord (AlgorithmResult PersonFeatures) where
  toNamedRecord AlgorithmResult {..} =
    namedRecord $
      HM.toList (toNamedRecord input)
        <> ["LienWikidata" .= predictedValue, "Verifie" .= ("0" :: Text)]

instance ToNamedRecord (AlgorithmResult (AnnotatedFeatures PersonFeatures)) where
  toNamedRecord AlgorithmResult {..} =
    namedRecord $
      HM.toList (toNamedRecord input)
        <> ["LienWikidataPredicted" .= predictedValue]

instance ToNamedRecord (AlgorithmResult (DatasetValue PersonFeatures)) where
  toNamedRecord AlgorithmResult {..} =
    namedRecord $
      HM.toList (toNamedRecord input)
        <> ["LienWikidataPredicted" .= predictedValue]

instance ToNamedRecord (AlgorithmResult FilmoFeatures) where
  toNamedRecord AlgorithmResult {..} =
    namedRecord $
      HM.toList (toNamedRecord input)
        <> ["LienWikidata" .= predictedValue, "Verifie" .= ("0" :: Text)]

instance ToNamedRecord (AlgorithmResult (AnnotatedFeatures FilmoFeatures)) where
  toNamedRecord AlgorithmResult {..} =
    namedRecord $
      HM.toList (toNamedRecord input)
        <> ["LienWikidataPredicted" .= predictedValue]

instance ToNamedRecord (AlgorithmResult (DatasetValue FilmoFeatures)) where
  toNamedRecord AlgorithmResult {..} =
    namedRecord $
      HM.toList (toNamedRecord input)
        <> ["LienWikidataPredicted" .= predictedValue]

instance DefaultOrdered (AlgorithmResult PersonFeatures) where
  headerOrder _ =
    headerOrder (undefined :: PersonFeatures)
      <> header ["LienWikidata", "Verifie"]

instance DefaultOrdered (AlgorithmResult (AnnotatedFeatures PersonFeatures)) where
  headerOrder _ =
    headerOrder (undefined :: (AnnotatedFeatures PersonFeatures))
      <> header ["LienWikidataPredicted"]

instance DefaultOrdered (AlgorithmResult (DatasetValue PersonFeatures)) where
  headerOrder _ =
    headerOrder (undefined :: (DatasetValue PersonFeatures))
      <> header ["LienWikidataPredicted"]

instance DefaultOrdered (AlgorithmResult FilmoFeatures) where
  headerOrder _ =
    headerOrder (undefined :: FilmoFeatures)
      <> header ["LienWikidata", "Verifie"]

instance DefaultOrdered (AlgorithmResult (AnnotatedFeatures FilmoFeatures)) where
  headerOrder _ =
    headerOrder (undefined :: (AnnotatedFeatures FilmoFeatures))
      <> header ["LienWikidataPredicted"]

instance DefaultOrdered (AlgorithmResult (DatasetValue FilmoFeatures)) where
  headerOrder _ =
    headerOrder (undefined :: (DatasetValue FilmoFeatures))
      <> header ["LienWikidataPredicted"]
