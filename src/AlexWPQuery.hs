{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables, NoImplicitPrelude, QuasiQuotes, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Using Alex's Wikipedia link extractor.
-- Copyright   :  (C) 2015-2016, Stephen J. Barr
-- License     :  None
-- Maintainer  :  Stephen J. Barr <stephen@planetbarr.com>
-- Stability   :  experimental
--
-- This is using Alex's Wikipedia link extractor to get links
-- mentioned in a particular Wikipedia page.
-- 
----------------------------------------------------------------------------

import ClassyPrelude
import Network.Wreq
import Control.Lens hiding ((.=))
import Data.Aeson --  (toJSON, pairs, (.=))
import Data.Aeson.Lens
import Data.Aeson.Encode.Pretty

import qualified Network.HTTP.Base as HB
import qualified Data.Maybe as M
import qualified Data.Text as T
import qualified Data.List as L

import System.Process


-- let jp_args :: [String] = ["-jar", "WikiLinks.jar" ,"https://en.wikipedia.org/wiki/Earth"]
-- el <-  readProcess "java" jp_args []

-- NOTE: Filter out /wiki/File: and /wiki/Help: links

link_prefixes_to_remove = ["/wiki/File:",  "/wiki/Help:"]

getLinkTextData :: Text -> IO (Maybe [(Text, Text)])
getLinkTextData input_wiki_url = do
  let jp_args :: [String] = ["-jar", "WikiLinks.jar" , (T.unpack input_wiki_url)]
  el <-  readProcess "java" jp_args []
  
  let run_success = L.head $ el ^.. key "run_success" . _Bool
  let words = el ^.. key "primary" . values . key "word" . _String
  let paths = el ^.. key "primary" . values . key "path" . _String
  let outs = zip words paths

  case run_success of
    False -> return Nothing
    True  -> return $ Just outs

  
