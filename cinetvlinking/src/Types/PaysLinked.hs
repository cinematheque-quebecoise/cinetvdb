{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Types.PaysLinked where

import RIO
import Data.Csv (DefaultOrdered(..), ToNamedRecord(..), (.=), namedRecord, header)

data Place = Location Text | SubLocation Text

data PaysLinked = PaysLinked
  { paysLinkedId :: Int64
  , paysLinkedTerme :: Text
  , paysLinkedWdLink :: Text
  -- , paysLinkedWdLabel :: Text
  }

instance ToNamedRecord PaysLinked where
  toNamedRecord PaysLinked {..} =
    namedRecord $ [ "PaysId" .= paysLinkedId
                  , "Terme" .= paysLinkedTerme
                  , "LienWikidata" .= paysLinkedWdLink
                  -- , "WdEtiquette" .= paysLinkedWdLabel
                  ]

instance DefaultOrdered PaysLinked where
    headerOrder _ = header [ "PaysId"
                           , "Terme"
                           , "LienWikidata"
                           -- , "WdEtiquette"
                           ]

