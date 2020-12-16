{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Types.AlgorithmResult where

import           Import
import           Types.AnnotatedFeatures
import           Types.PersonFeatures

import qualified Data.ByteString.Lazy    as BL
import           Data.Csv                (DefaultOrdered (..),
                                          FromNamedRecord (..), Header,
                                          ToNamedRecord (..), header,
                                          namedRecord, (.:), (.=))
import           Data.Csv.Streaming      (Records, decodeByName)
import qualified Data.HashMap.Strict     as HM

data AlgorithmResult i = AlgorithmResult
    { input          :: i
    , predictedValue :: Text
    }
    deriving (Show)

instance FromNamedRecord (AlgorithmResult PersonFeatures) where
  parseNamedRecord r =
    AlgorithmResult <$> parseNamedRecord r <*> (r .: "LienWikidata")

instance FromNamedRecord (AlgorithmResult (AnnotatedFeatures PersonFeatures)) where
  parseNamedRecord r =
    AlgorithmResult <$> parseNamedRecord r <*> (r .: "LienWikidataPredicted")

-- instance (ToNamedRecord i) => ToNamedRecord (AlgorithmResult i) where
--   toNamedRecord AlgorithmResult{..} =
--     namedRecord $ HM.toList (toNamedRecord input) <> ["LienWikidata" .= predictedValue, "Verifie" .= ("0" :: Text)]

instance ToNamedRecord (AlgorithmResult PersonFeatures) where
  toNamedRecord AlgorithmResult {..} =
    namedRecord
      $  HM.toList (toNamedRecord input)
      <> ["LienWikidata" .= predictedValue, "Verifie" .= ("0" :: Text)]

instance ToNamedRecord (AlgorithmResult (AnnotatedFeatures PersonFeatures)) where
  toNamedRecord AlgorithmResult {..} =
    namedRecord
      $  HM.toList (toNamedRecord input)
      <> ["LienWikidataPredicted" .= predictedValue]

instance DefaultOrdered (AlgorithmResult PersonFeatures) where
  headerOrder _ = headerOrder (undefined :: PersonFeatures)
    <> header ["LienWikidata", "Verifie"]

instance DefaultOrdered (AlgorithmResult (AnnotatedFeatures PersonFeatures)) where
  headerOrder _ = headerOrder (undefined :: (AnnotatedFeatures PersonFeatures))
    <> header ["LienWikidataPredicted"]

readAlgorithmTrainResults
  :: (MonadIO m)
  => FilePath
  -> m
       ( Either
           String
           ( Header
           , Records (AlgorithmResult (AnnotatedFeatures PersonFeatures))
           )
       )
readAlgorithmTrainResults fpath = do
  csvData <- liftIO $ BL.readFile fpath
  return $ decodeByName csvData

readAlgorithmResults
  :: (MonadIO m)
  => FilePath
  -> m (Either String (Header, Records (AlgorithmResult PersonFeatures)))
readAlgorithmResults fpath = do
  csvData <- liftIO $ BL.readFile fpath
  return $ decodeByName csvData

