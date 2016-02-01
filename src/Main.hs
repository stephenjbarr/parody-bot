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
import SpotSearch
import ParodyBotUtil


--------------------------------------------------------------------------------




--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "hello world"


  -- Define Weird Al, this was based on known information
  let weird_al = SpotArtist { artist_name = "Weird Al"
                            , artist_id   = "1bDWGdIC2hardyt55nlQgG"
                            , artist_uri  = "spotify:artist:1bDWGdIC2hardyt55nlQgG"
                            }

  -- Do oauth2 song and dance to get bearer tokens
  (oab, spot_auth) <- getOauth2Token
  
  -- Get Al's Albums, then tracks
  al_albums  <- getAlbumsForArtist spot_auth weird_al
  al_track_0 <- mapM (getTracksForAlbum spot_auth) al_albums
  let al_tracks :: [SpotTrack] = concat al_track_0

  -- For each track, lookup the explicit parodies
  let al_parodies_by_regexp :: [SpotTrack]  = filter (\x -> length (scan parodyRegex (track_name x)) > 0) al_tracks
  let p_extract = ( fromJust .  (fmap extractNameArtist) .    headMay .   (scan parodyRegex) . track_name)
  let originals_by_regexp = (fmap p_extract al_parodies_by_regexp)
  let parody_and_original  = makePossibleDict $ zip al_parodies_by_regexp originals_by_regexp

  
  let po_list = HMS.toList parody_and_original
  -- For each original, look it up.
  matched_tracks <- mapM (searchForTrack oab) (map snd po_list)


  ----------------------------------------
  -- CREATE THE PARODY LIST
  let po_track_mapping :: HMS.HashMap SpotTrack SpotTrack = makePossibleDict $ zip (map fst po_list) matched_tracks
  -- Filter out the duplicates
  let po_no_dupes :: [(SpotTrack, SpotTrack)] = L.nubBy (\x y -> (track_name (fst x)) == (track_name (fst y))) $ filter (\(a, b) -> (a /= b)) $ HMS.toList po_track_mapping
  

  ----------------------------------------
  -- POPULATE THE GRAPH

  -- For each song, add it to the graph, returning a NodePath. Construct: track_path_map = HashMap SpotTrack NodePath
  let all_tracks = al_tracks ++ (catMaybes matched_tracks)
  tp0 <- sequence $ map (addItem . SPTrack) all_tracks
  let track_path_map = HMS.fromList $ zip all_tracks tp0

  -- For each song, lookup artist, dedupe list, add artists to graph, 
     -- artist_paths HashMap SpotArtist NodePath     
  all_artists <- sequence $ map (getArtistForTrack spot_auth) all_tracks
  aa0 <- sequence $ map (addItem . SPArtist) all_artists               
  let artist_path_map = HMS.fromList $ zip all_artists aa0

-- For each [(SpotArtist, SpotTrack)], lookup (NodePath Track) (NodePath Artist), and create relatinship 
  _ <- sequence $ map (\(ax, tx) -> artistAuthoredTrackExisting artist_path_map track_path_map ax tx) $ zip all_artists all_tracks

  -- Iterate over the [(Parody, Original)] list, adding the "IS_PARODY_OF" relationship
  _ <- sequence $ map (\(px, ox) ->  addParodiedRelationshipExisting track_path_map px ox 1.0) $ po_no_dupes
  

  -- Iterate over this entire list, adding all of the tracks, with a parody certainty of 1
  -- _ <- sequence $ map (\(p,o) -> addParodiedRelationshipAllNew (SPTrack p) (SPTrack o) 1.0) po_no_dupes



  putStrLn "done"

  

