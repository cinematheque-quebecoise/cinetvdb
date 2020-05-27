{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments #-}
module Run.CountryLinking (run) where

import Util (sparqlEndpoint)
import Database.CineTv.Model

import Import
import Types.PaysLinked
import qualified Data.Text as Text
import qualified Data.Text.Internal.Builder as Text
import qualified Data.Text.Lazy.Encoding as Text (encodeUtf8)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Database.Esqueleto hiding (get)
import Data.Pool (Pool)
import Data.Csv (encodeDefaultOrderedByNameWith, EncodeOptions(..), defaultEncodeOptions)
import qualified Data.ByteString.Lazy as BL
import qualified Database.SPARQL.Protocol.Client as SPARQL
import NeatInterpolation
import Network.HTTP.Client (HttpException(..), responseBody)

run :: Pool SqlBackend
    -> RIO App ()
run pool = do
  logInfo "Linking 'Pays' table to Wikidata..."

  countries <- liftIO $ getCountries pool

  linkedCountries <- mapM linkCountry countries

  let myOptions = defaultEncodeOptions {
    encUseCrLf = False
  , encIncludeHeader = True
  }
  liftIO $ BL.writeFile "PaysWd.csv" $ encodeDefaultOrderedByNameWith myOptions $ linkedCountries

  logInfo "Completed linking!"

getCountries :: Pool SqlBackend
             -> IO [Entity Pays]
getCountries pool = do
  countryResults <- liftIO $ flip runSqlPersistMPool pool $ do
    select $
      distinct $
      from $ \pays -> do
      return pays

  return countryResults

linkCountry :: Entity Pays
            -> RIO App PaysLinked
linkCountry pays = do
  let parsedPaysTerme = parsePaysTerme paysTermeText
  let term = case parsedPaysTerme of Location t -> t
                                     SubLocation t -> t

  logInfo $ display $ "Linking "
                   <> term
                   <> " to Wikidata entity..."

  let isCountry = case parsedPaysTerme of
                    Location _ -> True
                    _ -> False
  let queryLazyBs = textToLazyBs $ query term isCountry
  handle httpExceptionHandler $ do
    response <- liftIO $ (SPARQL.select sparqlEndpoint queryLazyBs)
    let (SPARQL.SelectResult results) = responseBody response
    let paysLinkedM = getLinkedCountry results

    case paysLinkedM of
      Just paysLinked -> do
        return paysLinked
      Nothing -> do
        return $ PaysLinked paysTextId paysTermeText ""

  where
    paysTextId = Text.pack $ show $ fromSqlKey $ entityKey pays
    paysTermeText = paysTerme $ entityVal pays

    getLinkedCountry :: [Map Text SPARQL.RDFTerm] -> Maybe PaysLinked
    getLinkedCountry results = do
      guard $ length results < 5
      -- let uris = fmap ((\(SPARQL.IRI uri) -> uri) . fromJust . Map.lookup "place") results
      let uris = catMaybes $ fmap (getIRI . fromJust . Map.lookup "place") results
      guard $ length uris > 0
      return $ PaysLinked paysTextId paysTermeText $ Text.intercalate " ; " uris
      -- result <- getIfSoleResult results
      -- (SPARQL.IRI placeUri) <- Map.lookup "place" result
      -- (SPARQL.Literal placeLabel) <- Map.lookup "placeLabel" result
      -- return $ PaysLinked paysTextId paysTermeText placeUri
    textToLazyBs = Text.encodeUtf8 . Text.toLazyText . Text.fromText

    getIRI :: SPARQL.RDFTerm -> Maybe Text
    getIRI (SPARQL.IRI uri) = Just uri
    getIRI _ = Nothing

    httpExceptionHandler (HttpExceptionRequest _ _) = return $ PaysLinked paysTextId paysTermeText ""
    httpExceptionHandler (InvalidUrlException _ _) = return $ PaysLinked paysTextId paysTermeText ""

parsePaysTerme :: Text -> Place
parsePaysTerme t =
  if Text.isInfixOf " : " t
  then SubLocation $ getSubLocation $ normalizeTerm t
  else Location $ normalizeTerm t

  where
    getSubLocation = fromJust . listToMaybe . reverse . Text.splitOn " : "
    normalizeTerm = Text.intercalate " " . reverse . Text.splitOn ", "

query :: Text
      -> Bool
      -> Text
query placeLabel isCountry =
  [text|
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX wd: <http://www.wikidata.org/entity/>
PREFIX wdt: <http://www.wikidata.org/prop/direct/>
SELECT DISTINCT ?place ?placeLabel
WHERE
{
  ?place wdt:P31/wdt:P279* $wdType .
  { ?place rdfs:label "$normalizedName"@fr. } UNION
  { ?place rdfs:label "$normalizedName"@en. } UNION
  { ?place skos:altLabel "$normalizedName"@en. } UNION
  { ?place skos:altLabel "$normalizedName"@fr. } .

  SERVICE wikibase:label { bd:serviceParam wikibase:language "fr". }
} LIMIT 5
  |]
  where
    wdType = if isCountry then "wd:Q6256" else "wd:Q82794"
    normalizedName = Text.strip
                   $ (fromJust . listToMaybe)
                   $ reverse
                   $ Text.splitOn " : "
                   $ Text.intercalate " "
                   $ reverse
                   $ Text.splitOn "," placeLabel

