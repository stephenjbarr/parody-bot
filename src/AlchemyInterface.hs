{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables, NoImplicitPrelude, QuasiQuotes, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  AlchemyInterface
-- Copyright   :  (C) 2015-2016, Stephen J. Barr
-- License     :  None
-- Maintainer  :  Stephen J. Barr <stephen@planetbarr.com>
-- Stability   :  experimental
--
-- This is an interface to AlchemyAPI
----------------------------------------------------------------------------


module AlchemyInterface where

import           ClassyPrelude

import Network.Wreq
import Control.Lens hiding ((.=))
import Data.Aeson --  (toJSON, pairs, (.=))
import Data.Aeson.Lens


import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml as Y
import GHC.Generics
import qualified Data.Text as T
import qualified Text.Read as TR
-- import qualified Data.List as L

data AlchemyApiCred = AlchemyApiCred { alchemy_api_key :: String } deriving (Show, Generic)
instance FromJSON AlchemyApiCred

----------------------------------------

data AlchemyEntity = AlchemyEntity {
    ae__entity_name :: Text
  , ae__relevance   :: Double
  , ae__sentiment   :: Text -- at some point this should be an ordered sum type
  , ae__count       :: Integer  
  , ae__type        :: Text
  , ae__subtypes    :: [Text]
--   , ae__linked_data :: [Text]
} deriving (Show, Generic)

instance FromJSON AlchemyEntity
instance ToJSON   AlchemyEntity

----------------------------------------
-- READ THE API KEY FROM SETTINGS

getAlchemyCred :: FilePath -> IO AlchemyApiCred
getAlchemyCred fp = do
  content <- BS.readFile fp -- (4)
  let parsedContent = Y.decode content :: Maybe AlchemyApiCred -- (5)
  case parsedContent of
    Nothing             -> error "Could not parse config file."
    Just creds          -> return $ creds





----------------------------------------
-- DEFINE SOME ENDPOINTS

url_entities_endpt :: String
url_entities_endpt = "http://gateway-a.watsonplatform.net/calls/url/URLGetRankedNamedEntities"


----------------------------------------

-- | makeAE takes a Value type that should have everything needed to create
-- an 'AlchemyEntity' and tries to make this. The 'Maybe' type is there to
-- additional restrictions that are not caught by the field filtration in
-- the 'getEntities' function.

makeAE :: Value -> Maybe AlchemyEntity
-- makeAE = error "undefined"
makeAE v = do
  v_type      <- v ^? key "type"      . _String

  v_relevance <- v ^? key "relevance" . _String
  let v_rel_d :: Double = TR.read (T.unpack v_relevance)

  v_sentiment <- v ^? key "sentiment" . key "type"  . _String

  v_count     <- v ^? key "count"     . _String
  let v_count_i :: Integer = TR.read (T.unpack v_count)

  -- v_text      <- v ^? key "text"  
  v_disam_name <- v ^? key "disambiguated" . key "name" . _String

  let v_subtypes = v ^.. key "disambiguated" . key "subType" . values . _String

  return $ AlchemyEntity v_disam_name v_rel_d v_sentiment v_count_i v_type v_subtypes

----------------------------------------

-- debug shit
-- creds <- getAlchemyCred "keys.yml"
-- let page_url :: Text = "https://en.wikipedia.org/wiki/French_Revolution"



getEntities :: AlchemyApiCred -> Text -> IO [AlchemyEntity]
getEntities creds page_url = do
  let my_key    = T.pack $ alchemy_api_key creds
  let my_params = defaults & param "apikey" .~ [my_key] & param "outputMode" .~ ["json"] & param "url" .~ [page_url] & param "sentiment" .~ ["1"]
  r <- getWith my_params url_entities_endpt
  let my_status  = r ^.. responseBody . key "status"   . _String
  let valid_objs = r ^.. responseBody . key "entities" . _Array . traversed . filtered (\v ->    (isJust $ v ^? key "type")
                                                                                              && (isJust $ v ^? key "relevance" )
                                                                                              && (isJust $ v ^? key "sentiment" . key "type")
                                                                                              && (isJust $ v ^? key "count")
                                                                                              && (isJust $ v ^? key "text")
                                                                                              && (isJust $ v ^? key "disambiguated")
                                                                                              && (isJust $ v ^? key "disambiguated" . key "name")
                                                                                              && (isJust $ v ^? key "disambiguated" . key "subType")
                                                                                       )
  let ae_objs = catMaybes $ map makeAE valid_objs
  return $ ae_objs



