{-# LANGUAGE NoImplicitPrelude #-}
module Types.Resource
  ( ResourceUri(..)
  )
where

import           Import

newtype ResourceUri = ResourceUri { unResourceUri :: Text } deriving (Eq, Ord)

instance Show ResourceUri where
  show (ResourceUri uri) = show uri
