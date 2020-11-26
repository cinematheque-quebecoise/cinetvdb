{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Run.NomLinking.Algorithm
  ( runAlgorithm
  )
where

import           Control.Monad.Trans.Maybe
import qualified Data.List                     as List
import qualified Data.Map                      as Map
import qualified Data.Text                     as Text
import           Import
import           Network.HTTP.Client           (HttpException (..))
import           Run.NomLinking.Model          (GetPersonRoles (..),
                                                GetUsersByName (..))
import           Run.NomLinking.Model.Wikidata (GetWikidataPersonMatch (..),
                                                GetWikidataPersonsCount (..),
                                                functionWdLinking)
import           Types.PersonFeatures

runAlgorithm
  :: ( MonadUnliftIO m
     , GetUsersByName m
     , GetPersonRoles m
     , GetWikidataPersonsCount m
     , GetWikidataPersonMatch m
     )
  => PersonFeatures
  -> m (Maybe Text)
runAlgorithm personFeatures = handle httpExceptionHandler $ runMaybeT $ do
  personFunctions        <- getPersonFunctions personFeatures
  -- liftIO $ Text.putStrLn $ query personName personFunctions

  hasHomonymousPersonsWd <- lift $ getHasHomonymousPersonsWd personFeatures
  -- lift $ Text.putStrLn $ Text.pack $ show hasHomonymousPersonsWd
  guard $ not hasHomonymousPersonsWd

  hasHomonymousPersonsCq <- lift $ getHasHomonymousPersonsCq personFeatures
  -- lift $ Text.putStrLn $ Text.pack $ show hasHomonymousPersonsCq
  guard $ not hasHomonymousPersonsCq

  MaybeT $ getMatchByNameAndRoles (getPersonName personFeatures) personFunctions

 where
  httpExceptionHandler (HttpExceptionRequest _ _) = return Nothing
  httpExceptionHandler (InvalidUrlException  _ _) = return Nothing

getPersonFunctions :: (GetPersonRoles m) => PersonFeatures -> MaybeT m [Text]
getPersonFunctions personFeatures = do
  personIds <- lift $ getPersonRoles $ personId personFeatures
  --                       $ personId personFeatures
  let personFunctions =
        List.nub $ concat $ mapMaybe (`Map.lookup` functionWdLinking) personIds
  case personFunctions of
    [] -> MaybeT $ return Nothing
    _  -> MaybeT $ return $ Just personFunctions

getHasHomonymousPersonsCq :: (GetUsersByName m) => PersonFeatures -> m Bool
getHasHomonymousPersonsCq personFeatures = do
  persons <- getUsersByName (firstname personFeatures) (lastname personFeatures)
  return $ length persons > 1

getHasHomonymousPersonsWd
  :: (GetWikidataPersonsCount m) => PersonFeatures -> m Bool
getHasHomonymousPersonsWd personFeatures = do
  countMaybe <- getCountsByFullname personName
  return $ maybe False (1 <) countMaybe
  where personName = getPersonName personFeatures

getPersonName :: PersonFeatures -> Text
getPersonName personFeatures =
  Text.strip $ firstname personFeatures <> " " <> lastname personFeatures
