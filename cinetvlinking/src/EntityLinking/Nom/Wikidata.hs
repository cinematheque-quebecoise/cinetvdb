{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module EntityLinking.Nom.Wikidata
  ( LinkedPerson(..)
  , linkNomId
  )
where

import           Database.CQ                          (MonadGetPersonName (..),
                                                           MonadGetPersonRoles (..),
                                                           PersonName (..))
import           Database.SPARQL.Protocol.Client.Extended (MonadSparqlQuery (..),
                                                           getIRI, selectQuery)
import           EntityLinking.Fonction.Wikidata          (roleWdLinking)
import           Import

import           Control.Monad.Except
import           Data.Either.Combinators                  (eitherToError,
                                                           maybeToRight)
import qualified Data.List                                as L
import qualified Data.List.NonEmpty                                as L hiding (nub)
import qualified Data.Map                                 as M
import qualified Data.Set                                 as S
import qualified Data.Text                                as T
import           NeatInterpolation
-- import EntityLinking.Types (LinkingException (EntityNotFoundCineTVException, EntityNotFoundWikidataException, MultiMatchException))
import EntityLinking.Nom.Types (PersonId)
import EntityLinking.Types (ResourceUri(..), LinkingException(LinkingException))

data LinkedPerson = LinkedPerson
    { linkedPersonName :: PersonName
    , linkedPersonUri  :: ResourceUri
    }

data PersonLinkingException = PersonNotFoundCineTVException PersonId
    | PersonNotFoundWikidataException PersonName
    | NoSharedRoleException PersonName [ResourceUri] [ResourceUri]
    | MultiMatchException [ResourceUri]
    deriving (Typeable)

instance Show PersonLinkingException where
  show (PersonNotFoundCineTVException nomId) = concat
    [ "La personne avec l'identificant "
    , show nomId
    , " n'existe pas dans CineTV."
    ]
  show (PersonNotFoundWikidataException personName) = concat
    ["Aucune personne nommée ", show personName, " existe dans Wikidata."]
  show (NoSharedRoleException personName personUris []) = concat
    [ show personName
    , " existe dans Wikidata, mais cette personne ne partage pas de rôles avec ceux de CineTV.\n"
    , "Voici les URIs possibles: "
    , L.intercalate ", " (show <$> personUris)
    , "\nAucun rôle connu."
    ]
  show (NoSharedRoleException personName personUris personRoleUris) = concat
    [ show personName
    , " existe dans Wikidata, mais cette personne ne partage pas de rôles avec ceux de CineTV.\n"
    , "Voici les URIs possibles: "
    , L.intercalate ", " (show <$> personUris)
    , "\nVoici les rôles connus: "
    , L.intercalate ", " (show <$> personRoleUris)
    ]
  show (MultiMatchException uris) =
    "Plusieurs liens possibiles existe dans Wikidata: "
      ++ L.intercalate ", " (show <$> uris)

instance Exception PersonLinkingException

linkNomId
  :: (MonadGetPersonRoles m, MonadGetPersonName m, MonadSparqlQuery m)
  => PersonId
  -> m (Either LinkingException ResourceUri)
linkNomId nomId = do
  personNameM    <- getPersonName nomId
  personRoleUris <- getPersonRoleUris nomId

  runExceptT $ do
    personName <- eitherToError
      $ maybeToRight (LinkingException $ PersonNotFoundCineTVException nomId) personNameM
    linkByPersonNameAndRoles personName personRoleUris

getPersonRoleUris :: (MonadGetPersonRoles m) => PersonId -> m [ResourceUri]
getPersonRoleUris nomId = do
  personRoleIds <- getPersonRoles nomId
  return
    $   fmap ResourceUri
    $   L.nub
    $   concat
    $   catMaybes
    $   (`M.lookup` roleWdLinking)
    <$> personRoleIds

linkByPersonNameAndRoles
  :: (MonadSparqlQuery m)
  => PersonName
  -> [ResourceUri]
  -> ExceptT LinkingException m ResourceUri
linkByPersonNameAndRoles personName personRoleUris = do
  possiblePeople <- selectPeopleByNameAndRoles personName personRoleUris
  case possiblePeople of
    []          -> throwError $ LinkingException $ PersonNotFoundWikidataException personName
    [personUri] -> return personUri
    people      -> throwError $ LinkingException $ MultiMatchException people

selectPeopleByNameAndRoles
  :: (MonadSparqlQuery m)
  => PersonName
  -> [ResourceUri]
  -> ExceptT LinkingException m [ResourceUri]
selectPeopleByNameAndRoles personName personRoleUris = do
  let personNames = T.splitOn "_x000D_\n"
        $ T.strip
        $ T.intercalate " "
        $ catMaybes [personFirstName personName, personLastName personName]

  let personFullNameM = L.maximumBy (\x y -> compare (T.length x) (T.length y)) <$> L.nonEmpty personNames

  case personFullNameM of
    Just personFullName -> do
      result <- lift $ selectQuery
        "https://query.wikidata.org/sparql"
        (encodeUtf8 $ mkSparqlQuery personFullName personRoleUris)

      let personUris = S.fromList $ mapMaybe
            (M.lookup "person" >=> getIRI >=> Just . ResourceUri)
            result
      let personUrisWithRole =
            S.fromList
              $ mapMaybe (M.lookup "person" >=> getIRI >=> Just . ResourceUri)
              $ filter (isJust . M.lookup "role") result
      let personUrisNoRole =
            S.toList $ S.difference personUris personUrisWithRole

      when (null personUrisWithRole && not (null personUrisNoRole))
        $ throwError $ LinkingException $ NoSharedRoleException personName personUrisNoRole personRoleUris

      return $ S.toList personUrisWithRole
    Nothing -> return []

mkSparqlQuery :: Text -> [ResourceUri] -> Text
mkSparqlQuery name personRoleUris =
  let escapedName = T.replace "\"" "\\\"" name
  in  [text|PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX wd: <http://www.wikidata.org/entity/>
PREFIX wdt: <http://www.wikidata.org/prop/direct/>
SELECT DISTINCT ?person ?role
WHERE
{
  # ?person wdt:P106/wdt:P31* wd:Q4220920 .
  ?person wdt:P31 wd:Q5 .
  { ?person rdfs:label "$escapedName"@fr. } UNION
  { ?person rdfs:label "$escapedName"@en. } UNION
  { ?person skos:altLabel "$escapedName"@en. } UNION
  { ?person skos:altLabel "$escapedName"@fr. } .

  OPTIONAL {
    $roleQueryPart
  }
}|]

 where
  roleQueryPart = T.intercalate " UNION " $ fmap roleToQuery personRoleUris

  roleToQuery uri =
    "{ ?person wdt:P106 ?role . bind(<" <> unResourceUri uri <> "> as ?role) }"
