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
import NeoParodyInterface
import AlexWPQuery          as A
import WikipediaSearch      as W
import AlchemyInterface     as AI

main :: IO ()
main = do
  print "Fuck yes"

  -- for every track,
  --   (1) use WikipediaSearch to get a URL for the search string "$track_title, $track_author"
  --   (2) use AlexWPQuery to get the intro links for each URL, called intro_links_for_track
  --   (3) for each intro_links_for_track=
  --       (i)   check if each track has :HAS_WP_PAGE link,
  --       (ii)  for those that do not, create (track:Track)-[:HAS_WP_PAGE]-(wpp:WPPage)
  --       (iii) for the (wpp:WPPage) link, set attribute processed = false

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
