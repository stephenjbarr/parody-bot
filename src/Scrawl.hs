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
import AlexWPQuery          as A
import WikipediaSearch      as W
import AlchemyInterface     as AI
import DuckDuckGoSearch     as DDG

----------------------------------------
import qualified Database.Neo4j.Transactional.Cypher as TC


----------------------------------------

trackWikipediaURLFinder :: TrackName -> IO (Maybe Text)
trackWikipediaURLFinder tn = do
  w0    <- W.bestMatchWikipediaURL      $ tn ++ " (song)"
  tn_r0 <- DDG.bestDDGMatchWikipediaURL $ tn ++ " (song)"
  -- tn_r1 <- DDG.bestDDGMatchWikipediaURL $ tn ++ " song"
  -- tn_r2 <- DDG.bestDDGMatchWikipediaURL $ tn ++ " song wikipedia"
  -- return $ maximumMay $ catMaybes $ [w0, tn_r0, tn_r1, tn_r2]
  return $ maximumMay $ catMaybes $ (take 3 (repeat w0)) ++ [ tn_r0]

----------------------------------------


main :: IO ()
main = do
  print "Fuck yes"

  -- for every track which does not have a HAS_WP_PAGE link
  --   (1) use WikipediaSearch to get a URL for the search string "$track_title, $track_author"
  --   (2) use AlexWPQuery to get the intro links for each URL, called intro_links_for_track
  --   (3) for each intro_links_for_track=
  --       (i)   REDACTED
  --       (ii)  CREATE (track:Track)-[:HAS_WP_PAGE]-(wpp:WPPage)
  --       (iii) For (wpp:WPPage) link, set attribute processed = false

  up_x <- runSuccess getAllTracksWithoutWPQuery
  let u_tracks    = (extractSpotTracks . allFirstElts . TC.vals) up_x
  let track_names = map track_name u_tracks
  let tnsearch    = map (\t -> t ++ " weird al") track_name
  -- best_urls      <- sequence $ map trackWikipediaURLFinder tnsearch

  -- links_for_term <- sequence $ map getLinkTextData best_urls

  -- for every unprocesed (track:Track)-[:HAS_WP_PAGE]-(wpp:WPPage)
  --   (1) Run AlchemyInterface to get extracted entities
  --       Add Neo4j node, add the raw wikitext, this will be useful for factor construction
  --       Add number of extracted entities.
  --   (2) Look through entities of type
  --          Person       -> MusicalArtist
  --          Organization -> MusicalGroup
  --   (3) Match these entities to SpotTrack and SpotArtist using Spotify.
  --       TODO: Write this fucking function.
  --   (4) Check to see if these are already in Neo, and if not add them.
  print "Fuck yeah!, done you fuckers!"
