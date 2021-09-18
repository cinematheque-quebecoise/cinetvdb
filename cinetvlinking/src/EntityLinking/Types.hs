{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExistentialQuantification #-}

module EntityLinking.Types where

import Import

data LinkingException = forall e. Show e => LinkingException e

instance Show LinkingException where
    show (LinkingException e) = show e

instance Exception LinkingException

newtype ResourceUri = ResourceUri { unResourceUri :: Text } deriving (Eq, Ord)

instance Show ResourceUri where
  show (ResourceUri uri) = show uri

-- instance Show LinkingException where
--   show (EntityNotFoundCineTVException eid) = concat
--     [ "La personne avec l'identificant "
--     , show eid
--     , " n'existe pas dans CineTV."
--     ]
--   show (EntityNotFoundWikidataException personName) = concat
--     ["Aucune personne nommée ", show personName, " existe dans Wikidata."]
--   show (NoSharedRoleException personName personUris []) = concat
--     [ show personName
--     , " existe dans Wikidata, mais cette personne ne partage pas de rôles avec ceux de CineTV.\n"
--     , "Voici les URIs possibles: "
--     , L.intercalate ", " (show <$> personUris)
--     , "\nAucun rôle connu."
--     ]
--   show (NoSharedRoleException personName personUris personRoleUris) = concat
--     [ show personName
--     , " existe dans Wikidata, mais cette personne ne partage pas de rôles avec ceux de CineTV.\n"
--     , "Voici les URIs possibles: "
--     , L.intercalate ", " (show <$> personUris)
--     , "\nVoici les rôles connus: "
--     , L.intercalate ", " (show <$> personRoleUris)
--     ]
--   show (MultiMatchException uris) =
--     "Plusieurs liens possibiles existe dans Wikidata: "
--       ++ L.intercalate ", " (show <$> uris)
