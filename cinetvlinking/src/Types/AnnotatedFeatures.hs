{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Types.AnnotatedFeatures where

import Import
import Types.PersonFeatures
import Data.Csv (Header, DefaultOrdered(..), FromNamedRecord(..), ToRecord(..), record, ToNamedRecord(..), (.:), (.=), namedRecord)
import Data.Csv.Streaming (Records, decodeByName)
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as Vector

data AnnotatedFeatures i = AnnotatedFeatures { features :: i
                                             , output :: Text
                                             } deriving (Show)

instance FromNamedRecord (AnnotatedFeatures PersonFeatures) where
  parseNamedRecord r = AnnotatedFeatures
                   <$> parseNamedRecord r
                   <*> (r .: "LienWikidataAnnote")

instance (ToRecord i) => ToRecord (AnnotatedFeatures i) where
  toRecord AnnotatedFeatures{..} =
    toRecord features <> (record [ encodeUtf8 output ])

instance (ToNamedRecord i) => ToNamedRecord (AnnotatedFeatures i) where
  toNamedRecord AnnotatedFeatures{..} =
    namedRecord $ (HM.toList $ toNamedRecord $ features) <> ["LienWikidataAnnote" .= output]

instance DefaultOrdered (AnnotatedFeatures PersonFeatures) where
  headerOrder _ = headerOrder (undefined :: PersonFeatures) <> Vector.singleton "LienWikidataAnnote"

instance DefaultOrdered (AnnotatedFeatures (AnnotatedFeatures PersonFeatures)) where
  headerOrder _ = headerOrder (undefined :: AnnotatedFeatures PersonFeatures) <> Vector.singleton "LienWikidataAnnote"

fromUnannotated :: a -> AnnotatedFeatures a
fromUnannotated a = AnnotatedFeatures a ""

readAnnotatedPeopleData :: FilePath
                        -> RIO App (Either String (Header, Records (AnnotatedFeatures PersonFeatures)))
readAnnotatedPeopleData fpath = do
  csvData <- liftIO $ BL.readFile fpath
  return $ decodeByName csvData
