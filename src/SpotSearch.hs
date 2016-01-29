{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables, NoImplicitPrelude, QuasiQuotes, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches #-}

module SpotSearch where

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

------------------------------------------------------------------------------------------------------------------------

-- Bits of data for accessing the Spotify web service.
-- This should be in a config file somewhere
spot_endpt :: String
spot_endpt = "https://api.spotify.com/v1/"

spot_cli_id     :: Text = "ed8c0a8b345b41c59793a92f7a1984ee"
spot_cli_secret :: Text = "93f44712e4414dee8182a98cc1c6b184"
spot_req_sec :: Text    = spot_cli_id ++  ":" ++ spot_cli_secret


----------------------------------------
-- Lenses for common things to access

getItemNames :: AsValue body0 => Response body0 -> [T.Text]
getItemNames r =  r ^.. responseBody . key "items" . values . key "name" . _String

getItemIds :: AsValue body0 => Response body0 -> [T.Text]
getItemIds r =  r ^.. responseBody . key "items" . values . key "id" . _String

getItemUris :: AsValue body0 => Response body0 -> [T.Text]
getItemUris r =  r ^.. responseBody . key "items" . values . key "uri" . _String


getOauth2Token ::  IO (Auth, Options)
getOauth2Token = do
  -- oauth2 song and dance
  let sreq =  decodeUtf8 $  B64.encode $ encodeUtf8 spot_req_sec
  let bearer = "Basic " ++ (T.unpack sreq)
  let auth_opts = defaults & header "Authorization" .~ [encodeUtf8 (T.pack bearer)]
  auth_req <- postWith auth_opts "https://accounts.spotify.com/api/token" ["grant_type" := ("client_credentials" :: T.Text)]
  let access_token = auth_req ^. responseBody . key "access_token" . _String
  let oab = oauth2Bearer $ encodeUtf8 access_token
  let spot_auth = defaults & auth ?~ oab
  return $ (oab, spot_auth)


------------------------------------------------------------------------------------------------------------------------
-- Spotify Interactors -
-- TODO getAlbumsForArtist and getTracksForAlbum should be generalized

getAlbumsForArtist :: Options -> SpotArtist -> IO [SpotAlbum]
getAlbumsForArtist auth_opts sp_artist = do  
  r <- getWith auth_opts (spot_endpt ++ "artists/" ++ (T.unpack (artist_id sp_artist)) ++ "/albums") 
  let item_names = getItemNames r
  let item_ids   = getItemIds   r
  let item_uris  = getItemUris  r
  return $ zipWith3 SpotAlbum item_names item_ids item_uris

getTracksForAlbum :: Options -> SpotAlbum -> IO [SpotTrack]
getTracksForAlbum xopt sp_album = do
  let endpt = spot_endpt ++ "albums/" ++ (T.unpack (album_id sp_album)) ++ "/tracks?offset=0&limit=50&market=US"
  r <- getWith xopt endpt
  let track_names =  getItemNames r
  let track_ids   =  getItemIds   r
  let track_uris  =  getItemUris  r
  return $ zipWith3 SpotTrack track_names track_ids track_uris

searchForTrackOpts :: Auth -> (TrackName, ArtistName) -> Options
searchForTrackOpts oab (song_name, artist_name) = defaults & param "type" .~ ["track"] & param "q" .~ [song_name ++ " " ++ artist_name] & auth ?~ oab

searchForTrack :: Auth ->  (TrackName, ArtistName) -> IO (Maybe SpotTrack)
searchForTrack oab (song_name, artist_name) = do
  let my_opts = searchForTrackOpts oab (song_name, artist_name)
  let endpt   = spot_endpt ++ "search" 
  rn <- getWith my_opts endpt
  let rid   = rn ^.. responseBody . key "tracks" . key "items" . values . key "id" . _String
  let rname = rn ^.. responseBody . key "tracks" . key "items" . values . key "name" . _String
  let ruri  = rn ^.. responseBody . key "tracks" . key "items" . values . key "uri" . _String
  return $ headMay $ zipWith3 SpotTrack rname rid ruri

getMyUserID :: Options -> IO UserID
getMyUserID opts = do
  r <- getWith opts (spot_endpt ++ "me")
  return $ r ^. responseBody . key "id" . _String

makePlaylist :: Options -> T.Text -> IO (Maybe SpotPlaylist)
makePlaylist auth_opts playlist_name = do
  user_id <- getMyUserID auth_opts
  let endpt = spot_endpt ++ "users/" ++ (T.unpack user_id) ++ "/playlists"
  r <- postWith auth_opts endpt ["name" := playlist_name]
  let pl_names =  getItemNames r
  let pl_ids   =  getItemIds   r
  let pl_uris  =  getItemUris  r
  return $ headMay $ zipWith3 SpotPlaylist pl_names pl_ids pl_uris

addToPlaylist :: Options -> SpotPlaylist -> UriList -> IO Status
addToPlaylist auth_opts spl tracks = do
  user_id <- getMyUserID auth_opts
  let endpt = spot_endpt ++ "users/" ++ (T.unpack user_id) ++ "/playlists/" ++ (T.unpack (playlist_id spl)) ++ "/tracks"
  r <- postWith auth_opts endpt (toJSON tracks)
  return $  r ^. responseStatus


getArtistForTrack :: Options -> SpotTrack -> IO (SpotArtist)
getArtistForTrack = error "ndy"
