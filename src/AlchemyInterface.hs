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



import           ClassyPrelude

import Network.Wreq
import Control.Lens hiding ((.=))
import Data.Aeson --  (toJSON, pairs, (.=))
import Data.Aeson.Lens


import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml as Y
import GHC.Generics
import qualified Data.Text as T

data AlchemyApiCred = AlchemyApiCred { alchemy_api_key :: String } deriving (Show, Generic)
instance FromJSON AlchemyApiCred

----------------------------------------

data AlchemyEntity = AlchemyEntity {
    ae__entity_name :: Text
  , ae__relevance   :: Double
  , ae__sentiment   :: Text -- at some point this should be an ordered sum type
  , ae__type        :: Text
  , ae__subtypes    :: [Text]
  , ae__linked_data :: [Text]
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

-- debug shit
-- creds <- getAlchemyCred "keys.yml"
-- let page_url :: Text = "https://en.wikipedia.org/wiki/French_Revolution"


----------------------------------------
-- 

-- getEntities :: AlchemyApiCred -> Text -> IO [AlchemyEntity]
-- getEntities creds page_url = do
  let my_key    = T.pack $ alchemy_api_key creds
  let my_params = defaults & param "apikey" .~ [my_key] & param "outputMode" .~ ["json"] & param "url" .~ [page_url]
  r <- getWith my_params url_entities_endpt

  let my_status = r ^.. responseBody . key "status" . _String

  let e_vals = 

  let r_entity_name = r ^.. responseBody . key "entities" . values . key "disambiguated" . key "name" . _String
  let r_relevance   = r ^.. responseBody . key "entities" . values . key "relevance" . _Double
  let r_sentiment   = r ^.. responseBody . key "entities" . values . key "sentiment" . key "type" . _String
  let r_type        = r ^.. responseBody . key "entities" . values . key "type" . _String

  let st_arys       =  r ^.. responseBody . key "entities" . values . key "disambiguated" . key "subType" . _Array 
  let r_subtypes    = map (^.. traverse . _String) st_arys

  let r_linked_data = 




  
