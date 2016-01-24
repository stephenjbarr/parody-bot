{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables, NoImplicitPrelude, QuasiQuotes, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches #-}


-----------------------------------------------------------------------------
-- |
-- Module      :  NeoParodyInterface
-- Copyright   :  (C) 2015-2016, Stephen J. Barr
-- License     :  None
-- Maintainer  :  Stephen J. Barr <stephen@planetbarr.com>
-- Stability   :  experimental
--
-- This is the interface between Neo4j and the Parody Bot.
----------------------------------------------------------------------------

module NeoParodyInterface where

import           ClassyPrelude
import           Database.Neo4j       as N
import           Database.Neo4j.Graph as G
import           Database.Neo4j.Batch as B
import           Database.Neo4j.Types as NT
import qualified Database.Neo4j.Transactional.Cypher as TC
import           SpotTypes
import qualified Data.HashMap.Strict as HMS
import qualified Data.HashMap.Lazy   as HML
--------------------------------------------------------------------------------


-- | Port for Neo4j
neo_port :: Port
neo_port = 8676

-- | Hostname for neo4j server
neo_host :: Hostname
neo_host = "parody-bot.graphmods.com"
-- neo_host = "127.0.0.1"

-- | Credentials for neo4j server
neo_creds :: Credentials
neo_creds = ("neo4j","justeatit")

-- | An empty parameter list for the Neo4j Transact Cypher 'TC'
emptyParams :: TC.Params
emptyParams :: TC.Params =  HML.fromList [] 


-- | Run a Neo4j Query
run :: forall a. Neo4j a -> IO a
run = withAuthConnection neo_host neo_port neo_creds

--------------------------------------------------------------------------------


trackProps :: SpotTrack -> Properties
trackProps x = HMS.fromList ([  "name" |: (track_name x :: Text)
                              , "id"   |: (track_id   x :: Text)
                              , "uri"  |: (track_uri  x :: Text)
                              ])

artistProps :: SpotArtist -> Properties
artistProps x = HMS.fromList ([   "name" |: (artist_name x :: Text)
                                , "id"   |: (artist_id   x :: Text)
                                , "uri"  |: (artist_uri  x :: Text)
                                ])

albumProps :: SpotAlbum -> Properties
albumProps x = HMS.fromList ([    "name" |: (album_name x :: Text)
                                , "id"   |: (album_id   x :: Text)
                                , "uri"  |: (album_uri  x :: Text)
                                ])

playlistProps :: SpotPlaylist -> Properties
playlistProps x = HMS.fromList ([ "name" |: (playlist_name x :: Text)
                                , "id"   |: (playlist_id   x :: Text)
                                , "uri"  |: (playlist_uri  x :: Text)
                                ])


itemToProperties :: SpotItem -> Properties
itemToProperties si = case si of
 SPTrack x    ->  trackProps x
 SPArtist x   ->  artistProps x
 SPAlbum x    ->  albumProps x
 SPPlaylist x ->  playlistProps x
  

itemToLabels :: SpotItem -> [Label]
itemToLabels si = case si of
 SPTrack x    ->  ["Track"]
 SPArtist x   ->  ["Artist"]
 SPAlbum x    ->  ["Album"]
 SPPlaylist x ->  ["Playlist"]


-------------------------------------------------------------------------------- 


-- | For a SpotItem, create a NodeBatchIdentifier that can be inserted into the graph
-- as part of a batch operation. This is the function that should be exported, and
-- can use more specific helper functions if necessary.
-- The node needs to also have a label
-- itemToNode :: NodeBatchIdentifier a => SpotItem -> BatchState Identity ()
itemToNode item = do
  let item_props = itemToProperties item
  n0 <- B.createNode item_props 
  -- l  <- B.addLabels (itemToLabels item) n0
  return n0

--------------------------------------------------------------------------------

addParodiedRelationshipAllNew :: SpotItem -> SpotItem -> Double -> IO Graph
addParodiedRelationshipAllNew parody original certainty = run $ B.runBatch $ do
  p_node <- itemToNode parody
  p_lab  <- B.addLabels (itemToLabels parody) p_node

  o_node <- itemToNode original
  o_lab  <- B.addLabels (itemToLabels original) o_node

  let rel_prop = HMS.fromList [("certainty" |: (certainty ))]
  B.createRelationship "IS_PARODY_OF" rel_prop p_node o_node
  return ()

--------------------------------------------------------------------------------

addItem' :: SpotItem -> IO (NodePath)
addItem' si = run $ do
  let n_props = itemToProperties si
  n <- N.createNode n_props
  return $ nodePath n

  -- return n

-- addItem si = run $ do
--   n <- addItem' si
--   let n_labs  = itemToLabels si
--   N.addLabels n_labs n
--   return $ n

-- addItem :: SpotItem -> IO (NodeIdentifier a)
-- addItem si = run $ B.runBatch $ do
--   n <- itemToNode si
--   labs <- B.addLabels (itemToLabels si) n
--   return n

lookupItem :: SpotItem -> IO (Maybe Node)
lookupItem = error "ndy"

artistAuthoredTrack :: SpotArtist -> SpotAlbum -> IO Graph
artistAuthoredTrack = error "not defined yet"

addParodiedRelationship :: SpotTrack -> SpotTrack -> Double -> IO Graph
addParodiedRelationship original parody certainty = error "ndy"

addSameArticleRelationship :: SpotItem -> SpotItem  -> IO Graph
addSameArticleRelationship = error "ndy"
