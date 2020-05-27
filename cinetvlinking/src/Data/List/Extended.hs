{-# LANGUAGE NoImplicitPrelude #-}
module Data.List.Extended
  ( groupByKey
  ) where

import RIO
import qualified Data.List as List

groupByKey :: (Ord b)
           => (a -> b)
           -> [a]
           -> [[a]]
groupByKey keyFunc = ( List.groupBy isGroupedBy
                     . List.sortBy compareFunc
                     )
  where
    isGroupedBy a b = keyFunc a == keyFunc b
    compareFunc a b = keyFunc a `compare` keyFunc b
