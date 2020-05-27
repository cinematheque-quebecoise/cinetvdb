{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments #-}
module Run.FilmoLinking (run) where

import Util (sparqlEndpoint)

import Import
import Types.FilmoLinked
import qualified Data.Text.Internal.Builder as Text
import qualified Data.Text.Lazy.Encoding as Text (encodeUtf8)
import qualified Data.Map as Map
-- import Database.Esqueleto hiding (get, select)
-- import Data.Pool (Pool)
import Data.Csv (encodeDefaultOrderedByNameWith, DefaultOrdered, ToNamedRecord, EncodeOptions(..), defaultEncodeOptions)
import qualified Data.ByteString.Lazy as BL
import Database.SPARQL.Protocol.Client hiding (ask)
import NeatInterpolation
import Network.HTTP.Client (responseBody)
import System.FilePath (joinPath)

run :: Bool
    -> RIO App ()
run _ = do
  logInfo "Linking 'Filmo table to Wikidata..."

  linkedFilmos <- getWikidataLinkedFilmos

  let myOptions = defaultEncodeOptions { encUseCrLf = False
                                       , encIncludeHeader = True
                                       }

  outputdir <- fmap (optionsOutputDir . appOptions) ask
  let outputFpath = joinPath [outputdir, "FilmoWd.csv"]
  liftIO $ writeCsvFile outputFpath myOptions linkedFilmos

  logInfo "Completed linking Filmo table to Wikidata!"

getWikidataLinkedFilmos :: RIO App [FilmoLinked]
getWikidataLinkedFilmos = getWdResultSet >>= (return . getFilmosLinkedFromResults)

getWdResultSet :: RIO App SelectResult
getWdResultSet = do
  let query = textToLazyBs linkedFilmosQuery

  response <- liftIO $ (select sparqlEndpoint query)
  return $ responseBody response

  where
    textToLazyBs = Text.encodeUtf8 . Text.toLazyText . Text.fromText

getFilmosLinkedFromResults :: SelectResult
                           -> [FilmoLinked]
getFilmosLinkedFromResults (SelectResult results) =
  catMaybes $ fmap getFilmoLinkedFromResult results

getFilmoLinkedFromResult :: Map Text RDFTerm
                         -> Maybe FilmoLinked
getFilmoLinkedFromResult result = do
  filmoId <- fmap getRDFTermText $ Map.lookup "filmoId" result
  filmoUri <- fmap getRDFTermText $ Map.lookup "filmo" result
  return $ FilmoLinked filmoId filmoUri

getRDFTermText :: RDFTerm -> Text
getRDFTermText (IRI t) = t
getRDFTermText (Literal t) = t
getRDFTermText (LiteralLang t _) = t
getRDFTermText (LiteralType t _) = t
getRDFTermText (Blank t) = t

linkedFilmosQuery :: Text
linkedFilmosQuery =
  [text|
PREFIX wdt: <http://www.wikidata.org/prop/direct/>
SELECT DISTINCT ?filmo ?filmoId
WHERE
{
  ?filmo wdt:P4276 ?filmoId.
}
ORDER BY (LCASE(xsd:integer(?filmoId)))
  |]
-- getFilmos :: Pool SqlBackend
--           -> IO [Entity Pays]
-- getFilmos pool = do
--   liftIO $ flip runSqlPersistMPool pool $ do
--     select $
--       distinct $
--       from $ \filmo -> do
--       return filmo

-- linkFilmo :: Entity Filmo
--           -> RIO App PaysLinked
-- linkFilmo filmo = do
--   logInfo $ display $ "Linking "
--                    <> filmoTitle
--                    <> " to Wikidata entity..."

--   let queryLazyBs = textToLazyBs $ query filmoTextId
--   handle httpExceptionHandler $ do
--     response <- liftIO $ (SPARQL.select sparqlEndpoint queryLazyBs)
--     let (SPARQL.SelectResult results) = responseBody response
--     let paysLinkedM = getLinkedCountry results

--     case paysLinkedM of
--       Just paysLinked -> do
--         return paysLinked
--       Nothing -> do
--         return $ PaysLinked paysTextId paysTermeText ""

--   where
--     filmoTextId = Text.pack $ show $ fromSqlKey $ entityKey filmo
--     filmoTitle = filmoTitreOriginal $ entityVal filmo

--     getLinkedCountry :: [Map Text SPARQL.RDFTerm] -> Maybe PaysLinked
--     getLinkedCountry results = do
--       guard $ length results < 5
--       -- let uris = fmap ((\(SPARQL.IRI uri) -> uri) . fromJust . Map.lookup "place") results
--       let uris = catMaybes $ fmap (getIRI . fromJust . Map.lookup "place") results
--       guard $ length uris > 0
--       return $ PaysLinked paysTextId paysTermeText $ Text.intercalate " ; " uris
--     textToLazyBs = Text.encodeUtf8 . Text.toLazyText . Text.fromText

--     getIRI :: SPARQL.RDFTerm -> Maybe Text
--     getIRI (SPARQL.IRI uri) = Just uri
--     getIRI _ = Nothing

--     httpExceptionHandler (HttpExceptionRequest _ _) = return $ PaysLinked paysTextId paysTermeText ""
--     httpExceptionHandler (InvalidUrlException _ _) = return $ PaysLinked paysTextId paysTermeText ""

-- query :: Text
--       -> Bool
--       -> Text
-- query placeLabel isCountry =
--   [text|
-- PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
-- PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
-- PREFIX wd: <http://www.wikidata.org/entity/>
-- PREFIX wdt: <http://www.wikidata.org/prop/direct/>
-- SELECT DISTINCT ?place ?placeLabel
-- WHERE
-- {
--   ?place wdt:P31/wdt:P279* $wdType .
--   { ?place rdfs:label "$normalizedName"@fr. } UNION
--   { ?place rdfs:label "$normalizedName"@en. } UNION
--   { ?place skos:altLabel "$normalizedName"@en. } UNION
--   { ?place skos:altLabel "$normalizedName"@fr. } .

--   SERVICE wikibase:label { bd:serviceParam wikibase:language "fr". }
-- } LIMIT 5
--   |]
--   where
--     wdType = if isCountry then "wd:Q6256" else "wd:Q82794"
--     normalizedName = Text.strip
--                    $ (fromJust . listToMaybe)
--                    $ reverse
--                    $ Text.splitOn " : "
--                    $ Text.intercalate " "
--                    $ reverse
--                    $ Text.splitOn "," placeLabel

-- | Write rows to CSV file with header and a default header order.
writeCsvFile :: (DefaultOrdered a, ToNamedRecord a)
             => FilePath
             -> EncodeOptions
             -> [a]
             -> IO ()
writeCsvFile fpath options rows =
  BL.writeFile fpath $ encodeDefaultOrderedByNameWith options rows
