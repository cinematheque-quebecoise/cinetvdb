{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Run.NomLinking.Model.Wikidata
  ( GetWikidataPersonsCount(..)
  , GetWikidataPersonMatch(..)
  , functionWdLinking
  )
where

import           Data.Either.Combinators         (rightToMaybe)
import qualified Data.Map                        as Map
import qualified Data.Text                       as Text
import           Data.Text.ICU.Char              (Bool_ (Diacritic), property)
import           Data.Text.ICU.Normalize         (NormalizationMode (NFD),
                                                  normalize)
import qualified Data.Text.Internal.Builder      as Text
import qualified Data.Text.Lazy.Encoding         as Text (encodeUtf8)
import qualified Data.Text.Read                  as Text
import           Database.SPARQL.Protocol.Client
import           Import                          hiding ((^.))
import           NeatInterpolation
import           Network.HTTP.Client             (responseBody)
import qualified RIO.List                        as List

functionWdLinking :: Map.Map Text [Text]
functionWdLinking =
     -- Interprétation
  Map.singleton "15" ["Q2259451", "Q10800557", "Q10798782", "Q33999"]
     -- Scénario
     -- > screenwriter
    <> Map.singleton "31" ["Q28389"]
     -- Réalisation
     -- > film director
    <> Map.singleton "realisation" ["Q2526255"]
     -- Assistant réalisation
     -- > film director
    <> Map.singleton "3" ["Q2526255"]
     -- Producteur
     -- > film producer
    <> Map.singleton "28" ["Q3282637"]
     -- Producteur exécutif
     -- > film producer
    <> Map.singleton "48" ["Q3282637"]
     -- Producteur délégué
     -- > film producer
    <> Map.singleton "29" ["Q3282637"]
     -- Direction de production
     -- > film producer
    <> Map.singleton "11" ["Q3282637"]
     -- Montage image
    <> Map.singleton "19" ["Q7042855"]
     -- Image
    <> Map.singleton "14" ["Q7042855"]
     -- Musique
     -- > composer, singer, record producer, musician
    <> Map.singleton "21" ["Q36834", "Q177220", "Q183945", "Q639669"]
     -- Interprète musique
    <> Map.singleton "16" ["Q36834", "Q177220", "Q183945", "Q639669"]
     -- Montage sonore
    <> Map.singleton "20" ["Q36834", "Q177220", "Q183945", "Q639669"]
     -- Mixage
    <> Map.singleton "18" ["Q36834", "Q177220", "Q183945", "Q639669"]
     -- Prise de son
    <> Map.singleton "27" ["Q36834", "Q177220", "Q183945", "Q639669"]
     -- Conception sonore
    <> Map.singleton "7" ["Q36834", "Q177220", "Q183945", "Q639669"]
     -- Direction artistique
    <> Map.singleton "10" []
     -- Scripte
    <> Map.singleton "32" []
     -- Photographe de plateau
    <> Map.singleton "26" []
     -- Maquillage
    <> Map.singleton "17" []
     -- Costumes
     -- > Costume designer
    <> Map.singleton "8" ["Q1323191"]
     -- Recherche
    <> Map.singleton "38" []
     -- Animation
    <> Map.singleton "2" ["Q266569"]
     -- Décors
    <> Map.singleton "9" []
     -- Narrateur
    <> Map.singleton "23" []
     -- Narration
    <> Map.singleton "22" []
     -- Source originale
    <> Map.singleton "33" []
     -- Participant
    <> Map.singleton "24" []
     -- Régie
    <> Map.singleton "30" []
     -- Effets spéciaux
    <> Map.singleton "12" []
     -- Coiffure
    <> Map.singleton "6" []
     -- Conception
    <> Map.singleton "44" []
     -- Animateur d'émission
    <> Map.singleton "35" []
     -- Société de distribution
    <> Map.singleton "39" []
     -- Société de production
    <> Map.singleton "34" []
     -- Générique additionnel
    <> Map.singleton "13" []

class (Monad m) => GetWikidataPersonsCount m where
  getCountsByFullname :: Text -> m (Maybe Int)

class (Monad m) => GetWikidataPersonMatch m where
  getMatchByNameAndRoles :: Text -> [Text] -> m (Maybe Text)

instance GetWikidataPersonsCount (RIO env) where
  getCountsByFullname name = do
    response <- liftIO $ select sparqlEndpoint (textToLazyBs $ query name)
    let (SelectResult results) = responseBody response

    return
      $   listToMaybe results
      >>= Map.lookup "count"
      >>= getLiteral
      >>= convertToNumber

   where
    sparqlEndpoint =
      "https://query.wikidata.org/bigdata/namespace/wdq/sparql?query"
    convertToNumber countText =
      fmap fst $ rightToMaybe $ Text.decimal countText
    textToLazyBs = Text.encodeUtf8 . Text.toLazyText . Text.fromText
    query personName =
      [text|PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
  PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
  PREFIX wd: <http://www.wikidata.org/entity/>
  PREFIX wdt: <http://www.wikidata.org/prop/direct/>
  SELECT (count(distinct ?item ) as ?count)
  WHERE
  {
    { ?item rdfs:label "$personName"@fr. } UNION
    { ?item rdfs:label "$personName"@en. } UNION
    { ?item skos:altLabel "$canonicalName"@en. } UNION
    { ?item skos:altLabel "$canonicalName"@fr. } .
    $functionQueries
  }
        |]
    functionQueries =
      Text.intercalate " UNION " $ fmap functionToQuery allFunctions
    functionToQuery f = "{ ?item wdt:P106 wd:" <> f <> " }"
    allFunctions  = List.nub $ concat $ Map.elems functionWdLinking

    -- personName = getPersonName personFeatures
    canonicalName = getCanonicalForm name

instance GetWikidataPersonMatch (RIO env) where
  getMatchByNameAndRoles personName personFunctions = do
    response <- liftIO $ select sparqlEndpoint (textToLazyBs query)
    let (SelectResult results) = responseBody response

    return $ getIfSoleResult results >>= Map.lookup "item" >>= getIRI

   where
    sparqlEndpoint =
      "https://query.wikidata.org/bigdata/namespace/wdq/sparql?query"
    textToLazyBs = Text.encodeUtf8 . Text.toLazyText . Text.fromText
    query = [text|
      PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
      PREFIX wd: <http://www.wikidata.org/entity/>
      PREFIX wdt: <http://www.wikidata.org/prop/direct/>
      SELECT DISTINCT ?item
      WHERE
      {
        # ?item wdt:P106/wdt:P31* wd:Q4220920 .
        { ?item rdfs:label "$personName"@fr. } UNION
        { ?item rdfs:label "$personName"@en. } UNION
        { ?item skos:altLabel "$canonicalName"@en. } UNION
        { ?item skos:altLabel "$canonicalName"@fr. } .
        # { ?item wdt:P345 [] } UNION { ?item wdt:P2019 [] } UNION { ?item wdt:1266 [] } .
        $functionQueries
      } LIMIT 10
        |]
     where
      functionQueries =
        Text.intercalate " UNION " $ fmap functionToQuery personFunctions
      functionToQuery f = "{ ?item wdt:P106 wd:" <> f <> " }"
      canonicalName = getCanonicalForm personName


-- getWikidataLink :: PersonFeatures
--                 -> [Text]
--                 -> MaybeT IO Text
-- getWikidataLink personFeatures personFunctions = do
--   response <- lift $ select sparqlEndpoint (textToLazyBs $ query personName personFunctions)
--   let (SelectResult results) = responseBody response

--   firstResult <- MaybeT $ return $ getIfSoleResult results
--                                >>= Map.lookup "item"
--                                >>= getIRI

--   return firstResult
--   -- return $ Text.replace "http://www.wikidata.org/entity/" "" firstResult

--   where
--     personName = getPersonName personFeatures
--     textToLazyBs = Text.encodeUtf8 . Text.toLazyText . Text.fromText


getCanonicalForm :: Text -> Text
getCanonicalForm s = noAccents
 where
  noAccents      = Text.filter (not . property Diacritic) normalizedText
  normalizedText = normalize NFD s

getLiteral :: RDFTerm -> Maybe Text
getLiteral (Literal term) = Just term
getLiteral _              = Nothing

getIRI :: RDFTerm -> Maybe Text
getIRI (IRI iri) = Just iri
getIRI _         = Nothing

getIfSoleResult :: [a] -> Maybe a
getIfSoleResult [x] = Just x
getIfSoleResult _   = Nothing

