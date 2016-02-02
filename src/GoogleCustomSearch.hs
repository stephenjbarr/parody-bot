{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables, NoImplicitPrelude, QuasiQuotes, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GoogleCustomSearch
-- Copyright   :  (C) 2015-2016, Stephen J. Barr
-- License     :  None
-- Maintainer  :  Stephen J. Barr <stephen@planetbarr.com>
-- Stability   :  experimental
--
-- This is an interface to Google Custom Search Engine
----------------------------------------------------------------------------


module GoogleCustomSearch where

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

data GoogleCSECred = GoogleCSECred {
    google_key :: String
  , google_cse_id :: String
  } deriving (Show, Generic)
instance FromJSON GoogleCSECred

----------------------------------------
-- READ THE API KEY FROM SETTINGS

getGoogleCSECred :: FilePath -> IO GoogleCSECred
getGoogleCSECred fp = do
  content <- BS.readFile fp -- (4)
  let parsedContent = Y.decode content :: Maybe GoogleCSECred -- (5)
  case parsedContent of
    Nothing             -> error "Could not parse config file."
    Just creds          -> return $ creds



----------------------------------------
-- DEFINE SOME ENDPOINTS

url_cse_endpt :: String
url_cse_endpt = "https://www.googleapis.com/customsearch/v1"


----------------------------------------

-- debug shit
-- creds <- getGoogleCSECred "keys.yml"
-- let query = "weird al wikipedia"


getQueryUrls :: GoogleCSECred -> Text -> IO [Text]
getQueryUrls creds query = do
  let my_key    = T.pack $ google_key    creds
  let my_cse_id = T.pack $ google_cse_id creds
  let my_params = defaults & param "key" .~ [my_key] & param "cx" .~ [my_cse_id] & param "q" .~ [query]
  r <- getWith my_params url_cse_endpt
  let urls = r ^.. responseBody . key "items" . values . key "link" . _String
  return $ urls



