{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}
module Database.SPARQL.Protocol.Client.Extended
  ( MonadSparqlQuery(..)
  , getIRI
  , getLiteral
  )
where

import           Import                          hiding ((^.))

import           Data.Aeson
import           Database.SPARQL.Protocol.Client hiding (ask, select)
import           Network.HTTP.Client
import qualified Network.HTTP.Client.TLS         as HTTPS

class (Monad m) => MonadSparqlQuery m where
  selectQuery :: String -> ByteString -> m [Map Text RDFTerm]

instance MonadSparqlQuery IO where
  selectQuery url query = do
    response <- select url query
    let (SelectResult results) = responseBody response
    return results

instance MonadSparqlQuery (RIO env) where
  selectQuery url query = liftIO $ selectQuery url query

-- | Runs an SPARQL SELECT query.
select
  :: String                     -- ^ The URL of the server to run the query against.
  -> ByteString                 -- ^ An SPARQL SELECT query. It's sent as-is to the server.
  -> IO (Response SelectResult) -- ^ The result of the query.
select url query = do
  manager <- HTTPS.newTlsManager
  request <- parseRequest url
  let fullRequest = setQueryString
        [("query", Just query)]
        request
          { requestHeaders = [ ("Accept"    , "application/sparql-results+json")
                             , ("User-Agent", "haskell sparql-protocol-1.0")
                             ]
          }
  response <- httpLbs fullRequest manager

  case eitherDecode' (responseBody response) of
    Left  err -> fail err
    Right val -> return (fmap (const val) response)

getIRI :: RDFTerm -> Maybe Text
getIRI (IRI iri) = Just iri
getIRI _         = Nothing

getLiteral :: RDFTerm -> Maybe Text
getLiteral (Literal lit      ) = Just lit
getLiteral (LiteralLang lit _) = Just lit
getLiteral (LiteralType lit _) = Just lit
getLiteral _                   = Nothing
