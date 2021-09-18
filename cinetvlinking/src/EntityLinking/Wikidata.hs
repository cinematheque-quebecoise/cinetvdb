{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module EntityLinking.Wikidata
  ( MonadWikidataLinking (..),
  )
where

import Database.CQ (HasEntityId)
import EntityLinking.Filmo.Types (FilmoFeatures, FilmoId (..))
import EntityLinking.Filmo.Wikidata (linkFilmoId)
import EntityLinking.Nom.Types (PersonFeatures, PersonId (..))
import EntityLinking.Nom.Wikidata (linkNomId)
import EntityLinking.Types (LinkingException, ResourceUri)
import Import

class (Monad m, HasEntityId a) => MonadWikidataLinking m a where
  getWikidataLink :: Proxy a -> Int64 -> m (Either LinkingException ResourceUri)

instance (HasDbPool env) => MonadWikidataLinking (RIO env) PersonFeatures where
  getWikidataLink _ eid = linkNomId (PersonId eid)

instance (HasDbPool env) => MonadWikidataLinking (RIO env) FilmoFeatures where
  getWikidataLink _ eid = linkFilmoId (FilmoId eid)
