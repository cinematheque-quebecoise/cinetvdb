{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Types.FilmoLinked where

import RIO
import Data.Csv (DefaultOrdered(..), ToNamedRecord(..), (.=), namedRecord, header)

data FilmoLinked = FilmoLinked
  { filmoLinkedId :: Int64
  , filmoLinkedWdLink :: Text
  }

instance ToNamedRecord FilmoLinked where
  toNamedRecord FilmoLinked {..} =
    namedRecord $ [ "FilmoId" .= filmoLinkedId
                  , "LienWikidata" .= filmoLinkedWdLink
                  ]

instance DefaultOrdered FilmoLinked where
    headerOrder _ = header [ "FilmoId"
                           , "LienWikidata"
                           ]

