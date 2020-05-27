{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Types.AlgorithmResult where

import Import
import Types.PersonFeatures
import Types.AnnotatedFeatures

import Data.Csv (Header, DefaultOrdered(..), ToNamedRecord(..), FromNamedRecord(..), (.:), (.=), namedRecord, header)
import Data.Csv.Streaming (Records, decodeByName)
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM

data AlgorithmResult i = AlgorithmResult
  { input :: i
  , predictedValue :: Text
  } deriving (Show)

instance FromNamedRecord (AlgorithmResult (AnnotatedFeatures PersonFeatures)) where
  parseNamedRecord r = AlgorithmResult
                   <$> parseNamedRecord r
                   <*> (r .: "LienWikidata")

instance FromNamedRecord (AlgorithmResult PersonFeatures) where
  parseNamedRecord r = AlgorithmResult
                   <$> parseNamedRecord r
                   <*> (r .: "LienWikidata")

instance (ToNamedRecord i) => ToNamedRecord (AlgorithmResult i) where
  toNamedRecord AlgorithmResult{..} =
    namedRecord $ (HM.toList $ toNamedRecord $ input) <> ["LienWikidata" .= predictedValue]

instance DefaultOrdered (AlgorithmResult PersonFeatures) where
  headerOrder _ = headerOrder (undefined :: PersonFeatures) <> header ["LienWikidata"]

instance DefaultOrdered (AlgorithmResult (AnnotatedFeatures PersonFeatures)) where
  headerOrder _ = headerOrder (undefined :: (AnnotatedFeatures PersonFeatures)) <> header ["LienWikidata"]

readAlgorithmTrainResults :: (MonadIO m)
                          => FilePath
                          -> m (Either String (Header, Records (AlgorithmResult (AnnotatedFeatures PersonFeatures))))
readAlgorithmTrainResults fpath = do
  csvData <- liftIO $ BL.readFile fpath
  return $ decodeByName csvData

readAlgorithmResults :: (MonadIO m)
                     => FilePath
                     -> m (Either String (Header, Records (AlgorithmResult PersonFeatures)))
readAlgorithmResults fpath = do
  csvData <- liftIO $ BL.readFile fpath
  return $ decodeByName csvData

