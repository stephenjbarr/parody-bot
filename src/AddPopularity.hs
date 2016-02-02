{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables, NoImplicitPrelude, QuasiQuotes, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches #-}

module Main where

import ClassyPrelude
import Network.Wreq
import Control.Lens hiding ((.=))
import Data.Aeson --  (toJSON, pairs, (.=))
import Data.Aeson.Lens
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import Text.Regex.PCRE.Heavy as PH (Regex, re, scan)
import Data.Maybe (fromJust)

import qualified  Data.HashMap.Strict as HMS
import qualified  Data.List as L

import SpotTypes
import MyRegexes
import NeoParodyInterface   as N
import SpotSearch
import ParodyBotUtil

import qualified Data.ByteString.Lazy as BL
----------------------------------------
import qualified Database.Neo4j.Transactional.Cypher as TC


----------------------------------------


  
  
  

-- r <- runSuccess "match (a:Artist)-[:AUTHORED]-(t:Track) return a,t;"
-- let gs = TC.graph r
-- let g = Data.List.head gs
-- let items = extractItems g
-- let xs = HMS.elems items



main :: IO ()
main = do
  print "Fuck yes"

  -- get all tracks without popularity
  res <- runSuccess "match (t:Track) where not has(t.popularity) return t"
  let sp_map_list =  map  extractItems $ TC.graph res
  (oab, spot_auth) <- getOauth2Token
  let sp_map        = HMS.unions sp_map_list
  let sp_path_tracks =  catMaybes $ filterListJustSnd $ HMS.toList $  HMS.map extractSpotTrack sp_map


  -- For each track, make a request
  -- rs <- sequence $ map (searchEndptByID oab "tracks") $ map snd sp_path_tracks

  print "all have properties now fuckers"

  
