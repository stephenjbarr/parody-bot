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

import Control.Lens hiding ((.=))
import Data.Aeson --  (toJSON, pairs, (.=))
import Data.Aeson.Lens

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


runSuccess :: Text -> IO TC.Result
runSuccess query_text = do
  let query       = TC.cypher query_text emptyParams
  let transaction = TC.runTransaction query
  result          <- run $ transaction
  return $ TC.fromSuccess result

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

-- SAVING THIS LINE TO REMIND MYSELF THAT I AM AN IDIOT SOMETIMES.
item_types = ["Track", "Artist", "Album", "Playlist"]

-------------------------------------------------------------------------------- 


makeItem :: Text -> (Text, Text, Text) -> Maybe SpotItem
makeItem itype (n, i, u) = case itype of  
  "Track"    -> Just $ SPTrack    $ SpotTrack    n i u
  "Artist"   -> Just $ SPArtist   $ SpotArtist   n i u
  "Album"    -> Just $ SPAlbum    $ SpotAlbum    n i u
  "Playlist" -> Just $ SPPlaylist $ SpotPlaylist n i u
  _          -> Nothing

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

----------------------------------------

singleTextValueProperty :: PropertyValue -> Maybe Text
singleTextValueProperty pv = case pv of ValueProperty (TextVal x) -> Just x
                                        _ -> Nothing



nodeToItem :: Graph -> Node -> Maybe SpotItem
nodeToItem g n = do
  let n_path   = nodePath n
  let n_labs   = getNodeLabels n_path g
  pos_type <- headMay $ map fst $ filter (\x -> (snd x) == True) $ zip item_types $ map ((flip member) n_labs) item_types
  let n_props  = getNodeProperties n
  i_name   <- (lookup "name" n_props) >>= singleTextValueProperty
  i_id     <- (lookup "id" n_props)   >>= singleTextValueProperty
  i_uri    <- (lookup "uri" n_props)  >>= singleTextValueProperty
  makeItem pos_type (i_name, i_id, i_uri)

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

addParodiedRelationshipExisting :: HashMap SpotTrack NodePath -> SpotTrack -> SpotTrack -> Double -> IO Graph
addParodiedRelationshipExisting = error "ndy"

--------------------------------------------------------------------------------

addItem' :: SpotItem -> IO (NodePath)
addItem' si = run $ do
  let n_props = itemToProperties si
  n <- N.createNode n_props
  return $ nodePath n


addItem :: SpotItem -> IO (NodePath)
addItem si = do
  let n_labs  = itemToLabels si
  n <-  addItem' si
  _ <- run $ runBatch (B.addLabels n_labs n)
  return n


--------------------------------------------------------------------------------


allFirstElts :: [[a]] -> [a]
allFirstElts x = catMaybes $ map headMay x 

--------------------------------------------------------------------------------
--       
--        /$$$$$$                                /$$                    
--       /$$__  $$                              |__/                    
--      | $$  \ $$ /$$   /$$  /$$$$$$   /$$$$$$  /$$  /$$$$$$   /$$$$$$$
--      | $$  | $$| $$  | $$ /$$__  $$ /$$__  $$| $$ /$$__  $$ /$$_____/
--      | $$  | $$| $$  | $$| $$$$$$$$| $$  \__/| $$| $$$$$$$$|  $$$$$$ 
--      | $$/$$ $$| $$  | $$| $$_____/| $$      | $$| $$_____/ \____  $$
--      |  $$$$$$/|  $$$$$$/|  $$$$$$$| $$      | $$|  $$$$$$$ /$$$$$$$/
--       \____ $$$ \______/  \_______/|__/      |__/ \_______/|_______/ 
--            \__/                                                      
--                                                                      


-- getAllArtistTrackPairs :: IO [(SpotTrack, SpotArtist)]
-- getAllArtistTrackPairs = do
--   let q = "MATCH (t:Track)-[ar:AUTHORED]-(a:Artist) return t,ar,a"
--   g0 <- runSuccess q
--   let g = TC.graph g0
  




--------------------------------------------------------------------------------

--         /$$$$$$$$                           /$$                
--        |__  $$__/                          | $$                
--           | $$  /$$$$$$  /$$$$$$   /$$$$$$$| $$   /$$  /$$$$$$$
--           | $$ /$$__  $$|____  $$ /$$_____/| $$  /$$/ /$$_____/
--           | $$| $$  \__/ /$$$$$$$| $$      | $$$$$$/ |  $$$$$$ 
--           | $$| $$      /$$__  $$| $$      | $$_  $$  \____  $$
--           | $$| $$     |  $$$$$$$|  $$$$$$$| $$ \  $$ /$$$$$$$/
--           |__/|__/      \_______/ \_______/|__/  \__/|_______/ 
                                                        

getAllTracksQuery :: Text
getAllTracksQuery = "MATCH (t:Track) return t limit 10"

getAllTracksWithoutWPQuery :: Text
getAllTracksWithoutWPQuery = "MATCH (t:Track) OPTIONAL MATCH (t)-[r:HAS_WP_PAGE]-() WHERE r is null return t limit 10"


extractSpotTracks :: (Traversable t, AsValue a) => t a -> [SpotTrack]
extractSpotTracks v = zipWith3 SpotTrack t_names t_ids t_uris
  where
    t_names = v ^.. traverse . key "name"  . _String
    t_ids   = v ^.. traverse . key "id"   . _String
    t_uris  = v ^.. traverse . key "uri" . _String



--------------------------------------------------------------------------------

--          /$$$$$$              /$$     /$$             /$$             
--         /$$__  $$            | $$    |__/            | $$             
--        | $$  \ $$  /$$$$$$  /$$$$$$   /$$  /$$$$$$$ /$$$$$$   /$$$$$$$
--        | $$$$$$$$ /$$__  $$|_  $$_/  | $$ /$$_____/|_  $$_/  /$$_____/
--        | $$__  $$| $$  \__/  | $$    | $$|  $$$$$$   | $$   |  $$$$$$ 
--        | $$  | $$| $$        | $$ /$$| $$ \____  $$  | $$ /$$\____  $$
--        | $$  | $$| $$        |  $$$$/| $$ /$$$$$$$/  |  $$$$//$$$$$$$/
--        |__/  |__/|__/         \___/  |__/|_______/    \___/ |_______/ 


getAllArtistsQuery :: Text
getAllArtistsQuery = "MATCH (a:Artist) return a limit 10"

getAllArtistsWithoutWPQuery :: Text
getAllArtistsWithoutWPQuery = "MATCH (a:Artist) OPTIONAL MATCH (a)-[r:HAS_WP_PAGE]-() WHERE r is null return a limit 10"


extractSpotArtists :: (Traversable t, AsValue a) => t a -> [SpotArtist]
extractSpotArtists v = zipWith3 SpotArtist t_names t_ids t_uris
  where
    t_names = v ^.. traverse . key "name"  . _String
    t_ids   = v ^.. traverse . key "id"   . _String
    t_uris  = v ^.. traverse . key "uri" . _String
                                                               


--------------------------------------------------------------------------------

runGetAllTracksGraph :: IO TC.Result
runGetAllTracksGraph = do
  g <- runSuccess getAllTracksWithoutWPQuery
  return $ g

--------------------------------------------------------------------------------

lookupItem :: SpotItem -> IO (Maybe Node)
lookupItem = error "ndy"

artistAuthoredTrack :: SpotArtist -> SpotTrack -> IO Graph
artistAuthoredTrack = error "not defined yet"

artistAuthoredTrackExisting :: HashMap SpotArtist NodePath -> HashMap SpotTrack NodePath -> SpotArtist -> SpotTrack -> IO Graph
artistAuthoredTrackExisting = error "not defined yet"


addParodiedRelationship :: SpotTrack -> SpotTrack -> Double -> IO Graph
addParodiedRelationship original parody certainty = error "ndy"

addSameArticleRelationship :: SpotItem -> SpotItem  -> IO Graph
addSameArticleRelationship = error "ndy"


--------------------------------------------------------------------------------
-- -- DEBUG SHIT
--  g <- runSuccess getAllTracksWithoutWPQuery
--  let v = allFirstElts $ TC.vals g
