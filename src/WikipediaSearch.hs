{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables, NoImplicitPrelude, QuasiQuotes, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  WikipediaSearch
-- Copyright   :  (C) 2015-2016, Stephen J. Barr
-- License     :  None
-- Maintainer  :  Stephen J. Barr <stephen@planetbarr.com>
-- Stability   :  experimental
--
-- This is to get the first result in a Wikipedia search.
----------------------------------------------------------------------------

module WikipediaSearch where

import ClassyPrelude
import Network.Wreq
import Control.Lens hiding ((.=))
import Data.Aeson --  (toJSON, pairs, (.=))
import Data.Aeson.Lens

import qualified Network.HTTP.Base as HB
import qualified Data.Maybe as M
import qualified Data.Text as T

wiki_base  = "https://en.wikipedia.org/"
wiki_endpt = "https://en.wikipedia.org/w/api.php"

	
-- endpt https://en.wikipedia.org/w/api.php?action=query&list=search&format=json&srsearch=white%20and%20nerdy%20weird%20al

searchOptions :: Text -> Options
searchOptions search_string = defaults & param "action" .~ ["query"] & param "list" .~ ["search"] & param "format" .~ ["json"] & param "srsearch" .~ [search_string]


getTitles :: AsValue body0 => Response body0 -> [Text]
getTitles r = r ^.. responseBody . key "query" .  key "search" . values . key "title" . _String


bestMatchWikipediaURL :: Text -> IO Text
bestMatchWikipediaURL search_text = do
  r <- getWith (searchOptions search_text) wiki_endpt
  let t0 = M.fromJust $ headMay $ getTitles r
  let tu = T.replace " " "_" t0
  return $ wiki_base ++ (T.pack (HB.urlEncode (T.unpack (tu))))
