{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module EntityLinking.Dataset where

import qualified Control.Foldl as L
import qualified Data.ByteString.Lazy as BL
import Data.Csv (DefaultOrdered (..), EncodeOptions (encIncludeHeader, encUseCrLf), FromField (parseField), FromNamedRecord (parseNamedRecord), ToField (toField), ToNamedRecord (toNamedRecord), ToRecord (toRecord), defaultEncodeOptions, encodeDefaultOrderedByNameWith, namedRecord, record, (.:), (.=))
import Data.Csv.Streaming (decodeByName)
import qualified Data.HashMap.Strict as HM
import Data.List ((!!))
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Read as Text
import qualified Data.Vector as Vector
import EntityLinking.Filmo.Types (FilmoFeatures)
import EntityLinking.Nom.Types (PersonFeatures)
import Import
import RIO.Directory (createDirectoryIfMissing, doesFileExist)
import System.Random (Random (randomRIO))

newtype Dataset a = Dataset {unDataset :: [DatasetValue a]}
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Semigroup (Dataset a) where
  Dataset d1 <> Dataset d2 = Dataset (d1 <> d2)

data DatasetValue a = DatasetValue
  { -- | Type representing features of an entity
    features :: a,
    -- | Annotated entity based on features
    output :: Maybe Text,
    -- | Annotation was manually verified by a human
    isVerified :: IsVerified
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

data IsVerified = IsVerified | IsNotVerified deriving (Eq, Show)

instance (FromNamedRecord a) => FromNamedRecord (DatasetValue a) where
  parseNamedRecord r =
    DatasetValue
      <$> parseNamedRecord r
      <*> (r .: "LienWikidata")
      <*> (r .: "Verifie")

instance (ToRecord i) => ToRecord (DatasetValue i) where
  toRecord DatasetValue {..} =
    toRecord features <> record [maybe "" encodeUtf8 output]

instance (ToNamedRecord i) => ToNamedRecord (DatasetValue i) where
  toNamedRecord DatasetValue {..} =
    namedRecord $
      HM.toList (toNamedRecord features)
        <> ["LienWikidata" .= output, "Verifie" .= isVerified]

instance ToField IsVerified where
  toField IsVerified = "1"
  toField IsNotVerified = "0"

instance FromField IsVerified where
  parseField s =
    let intValMaybe = fst <$> eitherToMaybe (Text.decimal $ Text.decodeUtf8 s) :: Maybe Integer
     in maybe
          (pure IsVerified)
          (\i -> if i <= 0 then pure IsNotVerified else pure IsVerified)
          intValMaybe

instance DefaultOrdered (DatasetValue PersonFeatures) where
  headerOrder _ =
    headerOrder (undefined :: PersonFeatures)
      <> Vector.fromList ["LienWikidata", "Verifie"]

instance DefaultOrdered (DatasetValue FilmoFeatures) where
  headerOrder _ =
    headerOrder (undefined :: FilmoFeatures)
      <> Vector.fromList ["LienWikidata", "Verifie"]

numVerifiedInstances :: Dataset a -> Int
numVerifiedInstances (Dataset dataset) = sum $ isVerifiedToInt . isVerified <$> dataset
  where
    isVerifiedToInt IsVerified = 1
    isVerifiedToInt IsNotVerified = 0

numMissingAnnotations :: Dataset a -> Int
numMissingAnnotations (Dataset dataset) = sum $ isVerifiedToInt . isVerified <$> dataset
  where
    isVerifiedToInt IsVerified = 0
    isVerifiedToInt IsNotVerified = 1

getVerifiedInstances :: Dataset a -> Dataset a
getVerifiedInstances = Dataset . filter (\d -> IsVerified == isVerified d) . unDataset

-- | Select sample of SampleSize elements from 'Dataset'
--  Modify randomizer. The reservoir sampling does not shuffle if size = length tl
sampleDataset :: MonadIO m => Integer -> Dataset a -> m (Dataset a)
sampleDataset sampleSize dataset = do
  sample <- liftIO $ fmap (fromMaybe Vector.empty) foldSample
  shuffledSample <- liftIO $ shuffle $ Vector.toList sample
  return $ Dataset shuffledSample
  where
    foldSample = L.foldM (L.randomN (fromIntegral sampleSize)) (unDataset dataset)

shuffle :: [a] -> IO [a]
shuffle x =
  if length x < 2
    then return x
    else do
      i <- randomRIO (0, length x - 1)
      r <- shuffle (take i x ++ drop (i + 1) x)
      return (x !! i : r)

-- | Write records to file
writeDataset ::
  forall a m.
  (MonadIO m, DefaultOrdered (DatasetValue a), ToNamedRecord a) =>
  FilePath ->
  Dataset a ->
  Bool ->
  m ()
writeDataset fpath dataset willAppend = do
  fpathExists <- liftIO $ doesFileExist fpath

  let action =
        if fpathExists && willAppend
          then BL.appendFile fpath
          else BL.writeFile fpath

  let myOptions =
        defaultEncodeOptions
          { encUseCrLf = False,
            encIncludeHeader = not fpathExists || not willAppend
          }

  liftIO $ createDirectoryIfMissing True "data"
  liftIO $ action $ encodeDefaultOrderedByNameWith myOptions $ unDataset dataset

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe e = case e of
  Left _ -> Nothing
  Right v -> Just v

fromUnannotated :: a -> DatasetValue a
fromUnannotated a = DatasetValue a Nothing IsNotVerified

readDatasetFromCsv ::
  (FromNamedRecord a, MonadIO m) =>
  FilePath ->
  m (Either String (Dataset a))
readDatasetFromCsv fpath = do
  csvData <- liftIO $ BL.readFile fpath
  return $ (\(_, r) -> Dataset (toList r)) <$> decodeByName csvData

data T a = forall t. (Foldable t) => T {unT :: t a}

-- newtype T a = T { unT :: forall t. Num (t Int) => t a }

create :: (Semigroup (T Int)) => T Int
create = T [1] <> T [2]
