{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables, NoImplicitPrelude, QuasiQuotes, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  DuckDuckGoSearch
-- Copyright   :  (C) 2015-2016, Stephen J. Barr
-- License     :  None
-- Maintainer  :  Stephen J. Barr <stephen@planetbarr.com>
-- Stability   :  experimental
--
-- This is to get the first result in a Wikipedia search.
----------------------------------------------------------------------------

module DuckDuckGoSearch where

import ClassyPrelude
import Network.Wreq
import Control.Lens hiding ((.=))
import Data.Aeson --  (toJSON, pairs, (.=))
import Data.Aeson.Lens

import qualified Network.HTTP.Base as HB
import qualified Data.Maybe as M
import qualified Data.Text as T

ddg_base  = "https://api.duckduckgo.com/"


-- endpt https://en.wikipedia.org/w/api.php?action=query&list=search&format=json&srsearch=white%20and%20nerdy%20weird%20al

searchOptions :: Text -> Options
searchOptions search_string = defaults & param "format" .~ ["json"] & param "q" .~ [search_string]


getTitles :: AsValue body0 => Response body0 -> [Text]
getTitles r = r ^.. responseBody . key "query" .  key "search" . values . key "title" . _String

getAbstractURL :: AsValue body0 => Response body0 -> Maybe Text
getAbstractURL r = r ^? responseBody . key "AbstractURL" . _String


bestDDGMatchAbstractURL :: Text -> IO (Maybe Text)
bestDDGMatchAbstractURL search_text = do
  r <- getWith (searchOptions search_text) ddg_base
  let u = getAbstractURL r
  let rv = case u of        
        Just "" -> Nothing
        Just x  -> u
        Nothing -> Nothing
  return $ rv
