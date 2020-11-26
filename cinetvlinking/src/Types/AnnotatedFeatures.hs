{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Types.AnnotatedFeatures where

import qualified Data.ByteString.Lazy as BL
import           Data.Csv             (DefaultOrdered (..), FromField (..),
                                       FromNamedRecord (..), Header,
                                       ToField (..), ToNamedRecord (..),
                                       ToRecord (..), namedRecord, record, (.:),
                                       (.=))
import           Data.Csv.Streaming   (Records, decodeByName)
import qualified Data.HashMap.Strict  as HM
import qualified Data.Text.Encoding   as Text
import qualified Data.Text.Read       as Text
import qualified Data.Vector          as Vector
import           Import
import           RIO.Prelude.Types
import           Types.PersonFeatures

data AnnotatedFeatures i = AnnotatedFeatures
    { features    :: i
    , output      :: Text
    , isAnnotated :: Bool
    }
    deriving (Show)

instance FromNamedRecord (AnnotatedFeatures PersonFeatures) where
  parseNamedRecord r =
    AnnotatedFeatures
      <$> parseNamedRecord r
      <*> (r .: "LienWikidata")
      <*> (r .: "Verifie")

intToBool :: Int -> Bool
intToBool i = i > 0

instance (ToRecord i) => ToRecord (AnnotatedFeatures i) where
  toRecord AnnotatedFeatures {..} =
    toRecord features <> record [encodeUtf8 output]

instance (ToNamedRecord i) => ToNamedRecord (AnnotatedFeatures i) where
  toNamedRecord AnnotatedFeatures {..} =
    namedRecord
      $  HM.toList (toNamedRecord features)
      <> ["LienWikidata" .= output, "Verifie" .= isAnnotated]

instance DefaultOrdered (AnnotatedFeatures PersonFeatures) where
  headerOrder _ = headerOrder (undefined :: PersonFeatures)
    <> Vector.fromList ["LienWikidata", "Verifie"]

instance DefaultOrdered (AnnotatedFeatures (AnnotatedFeatures PersonFeatures)) where
  headerOrder _ = headerOrder (undefined :: AnnotatedFeatures PersonFeatures)
    <> Vector.fromList ["LienWikidata", "Verifie"]

instance FromField Bool where
  parseField s =
    let intValMaybe = fst <$> eitherToMaybe (Text.decimal $ Text.decodeUtf8 s)
    in  maybe (pure False)
              (\i -> if i <= 0 then pure False else pure True)
              intValMaybe

instance ToField Bool where
  toField True  = "1"
  toField False = "0"

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe e = case e of
  Left  _ -> Nothing
  Right v -> Just v

fromUnannotated :: a -> AnnotatedFeatures a
fromUnannotated a = AnnotatedFeatures a "" False

readAnnotatedPeopleData
  :: FilePath
  -> RIO
       App
       (Either String (Header, Records (AnnotatedFeatures PersonFeatures)))
readAnnotatedPeopleData fpath = do
  csvData <- liftIO $ BL.readFile fpath
  return $ decodeByName csvData
