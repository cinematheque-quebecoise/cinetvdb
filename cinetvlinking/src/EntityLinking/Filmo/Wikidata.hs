{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module EntityLinking.Filmo.Wikidata
  ( LinkedFilmo(..)
  , linkFilmoId
  )
where

import           Database.CineTv.Public.Model
import           Database.CQ                          (MonadGetPersonName (..),
                                                           MonadGetPersonRoles (..),
                                                           PersonName (..))
import           Database.SPARQL.Protocol.Client.Extended (getIRI, selectQuery)
import           EntityLinking.Fonction.Wikidata          (roleWdLinking)
import           Import hiding ((^.))

import           Control.Monad.Except
import           Data.Either.Combinators                  (eitherToError,
                                                           maybeToRight)
import qualified Data.List                                as L
-- import qualified Data.List.NonEmpty                                as L hiding (nub)
import qualified Data.Map                                 as M
import qualified Data.Set                                 as S
import qualified Data.Text                                as T
import           NeatInterpolation
-- import EntityLinking.Types (LinkingException (EntityNotFoundCineTVException, EntityNotFoundWikidataException, MultiMatchException))
import EntityLinking.Nom.Types (PersonId)
import EntityLinking.Types (ResourceUri(..), LinkingException(LinkingException))
import qualified EntityLinking.Filmo.Types as Types
import Data.RDF (Node(..))
import Prelude (print, putStrLn)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Database.Esqueleto

data LinkedFilmo = LinkedFilmo
    { linkedFilmoTitle :: Text
    , linkedFilmoUri  :: ResourceUri
    }

data FilmoLinkingException = FilmoNotFoundCineTVException Types.FilmoId
    | FilmoNotFoundWikidataException Text
    | MultiMatchException [ResourceUri]
    deriving (Typeable)

instance Show FilmoLinkingException where
  show (FilmoNotFoundCineTVException fid) = concat
    [ "L'oeuvre avec l'identificant "
    , show fid
    , " n'existe pas dans CineTV."
    ]
  show (FilmoNotFoundWikidataException title) = concat
    ["Aucune oeuvre nommée ", show title, " existe dans Wikidata."]
  show (MultiMatchException uris) =
    "Plusieurs liens possibiles existe dans Wikidata: "
      ++ L.intercalate ", " (show <$> uris)

instance Exception FilmoLinkingException

class Monad m => FilmoStore m where
    getFilmoTitles :: Int64 -> m [Text]
    getFilmoYear :: Int64 -> m (Maybe Int)
    getFilmoDirectors :: Int64 -> m [Text]

instance HasDbPool env => FilmoStore (RIO env) where
    getFilmoTitles fid = do
        pool <- getDbPool <$> ask
        results <-
            liftIO
            $ flip runSqlPersistMPool pool
            $ select
            $ distinct
            $ from
            $ \filmo -> do
                where_
                  ( filmo ^. FilmoId ==. valkey fid
                  )
                pure filmo
        case join $ filmoTitreOriginal . entityVal <$> listToMaybe results of
          Just title -> pure [title]
          Nothing -> pure []

    getFilmoYear fid = do
        pool <- getDbPool <$> ask
        results <-
            liftIO
            $ flip runSqlPersistMPool pool
            $ select
            $ distinct
            $ from
            $ \filmo -> do
                where_
                  ( filmo ^. FilmoId ==. valkey fid
                  )
                pure filmo
        pure $ join $ filmoAnneeSortie . entityVal <$> listToMaybe results

    getFilmoDirectors fid = do
        pool <- getDbPool <$> ask
        results <-
            liftIO
            $ flip runSqlPersistMPool pool
            $ select
            $ distinct
            $ from
            $ \(filmo, filmoRealisation, nom) -> do
                where_
                  (   filmo ^. FilmoId ==. valkey fid
                  &&. filmo ^. FilmoId ==. filmoRealisation ^. Filmo_RealisationFilmoId
                  &&. nom ^. NomId ==. filmoRealisation ^. Filmo_RealisationNomId
                  )
                pure (nom ^. NomPrenom, nom ^. NomNom)

        pure $ (\(Value p, Value n) -> Text.intercalate " " $ catMaybes [p, n]) <$> results

linkFilmoId
    :: (MonadIO m, FilmoStore m)
    => Types.FilmoId
    -> m (Either LinkingException ResourceUri)
linkFilmoId (Types.FilmoId fid) = do

    filmoTitles <- getFilmoTitles fid
    maybeFilmoYear <- getFilmoYear fid
    filmoDirectors <- getFilmoDirectors fid

    case maybeFilmoYear of
      Just filmoYear -> do
        results <- liftIO $ selectQuery
            "https://query.wikidata.org/sparql"
            (encodeUtf8 $ mkSparqlQuery filmoTitles filmoYear filmoDirectors)
        case listToMaybe $ mapMaybe (M.lookup "item" >=> getIRI >=> Just . ResourceUri) results of
          Just uri -> pure $ Right uri
          Nothing -> pure $ Left $ LinkingException $ FilmoNotFoundWikidataException ""
      Nothing -> pure $ Left $ LinkingException $ FilmoNotFoundWikidataException ""

mkSparqlQuery :: [Text] -> Int -> [Text] -> Text
mkSparqlQuery titles year directorsName =
  [text|PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX wd: <http://www.wikidata.org/entity/>
PREFIX wdt: <http://www.wikidata.org/prop/direct/>
SELECT DISTINCT ?item
WHERE
{
    ?item wdt:P31 wd:Q11424 .

    ?item wdt:P1476 ?itemTitle .
    FILTER ($titleFilterQueryPart)

    ?item wdt:P577 ?publicationDate .
    FILTER (
        (  ?publicationDate >= "$yearText-01-01T00:00:00Z"^^xsd:dateTime
        && ?publicationDate <= "$yearText-12-31T00:00:00Z"^^xsd:dateTime
        ) ||
        (  ?publicationDate >= "$yearBeforeText-01-01T00:00:00Z"^^xsd:dateTime
        && ?publicationDate <= "$yearBeforeText-12-31T00:00:00Z"^^xsd:dateTime
        ) ||
        (  ?publicationDate >= "$yearAfterText-01-01T00:00:00Z"^^xsd:dateTime
        && ?publicationDate <= "$yearAfterText-12-31T00:00:00Z"^^xsd:dateTime
        )
    )

    ?item wdt:P57 ?director .
    $directorQueryPart
} LIMIT 10|]

    where
        escapedDirectorNames = concat
                             $ fmap mkPossibleNames
                             $ fmap (T.replace "\"" "\\\"") directorsName
        escapedTitles = fmap (T.replace "\"" "\\\"") titles

        yearText = Text.pack $ show year
        yearBeforeText = Text.pack $ show $ year - 1
        yearAfterText = Text.pack $ show $ year + 1

        titleFilterQueryPart = Text.intercalate " || " $ fmap titleToFilterQuery escapedTitles
        titleToFilterQuery title =
            [text|lcase(str(?itemTitle)) = lcase("$title")|]

        directorQueryPart = Text.intercalate " UNION " $ fmap directorToQuery escapedDirectorNames
        directorToQuery name =
            [text|{ ?director rdfs:label "$name"@fr } UNION { ?director rdfs:label "$name"@en }|]

mkPossibleNames :: Text -> [Text]
mkPossibleNames name =
  case T.words name of
    [] -> []
    [n] -> [n]
    nameParts -> fmap T.unwords $ filter (\w -> L.length w > 1) $ L.subsequences nameParts
