{-# LANGUAGE NoImplicitPrelude #-}
module Util
  ( sparqlEndpoint
  , getIfSingle
  ) where

import RIO

sparqlEndpoint :: String
sparqlEndpoint = "https://query.wikidata.org/bigdata/namespace/wdq/sparql?query"

getIfSingle :: [a] -> Maybe a
getIfSingle (x:[]) = Just x
getIfSingle _ = Nothing
