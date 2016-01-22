{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables, NoImplicitPrelude, QuasiQuotes #-}
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
-- import qualified SpotTypes as S
import MyRegexes




--------------------------------------------------------------------------------


-- Bits of data for accessing the Spotify web service.
-- This should be in a config file somewhere
spot_endpt :: String
spot_endpt = "https://api.spotify.com/v1/"

spot_cli_id     :: Text     = "ed8c0a8b345b41c59793a92f7a1984ee"
spot_cli_secret :: Text = "93f44712e4414dee8182a98cc1c6b184"
spot_req_sec :: Text    = spot_cli_id ++  ":" ++ spot_cli_secret


--------------------------------------------------------------------------------
-- some simple data types

type UserID     = T.Text

type TrackName  = T.Text
type TrackID    = T.Text
type TrackURI   = T.Text

type ArtistName = T.Text
type ArtistID   = T.Text
type ArtistURI  = T.Text

type AlbumName  = T.Text
type AlbumID    = T.Text
type AlbumURI   = T.Text

type PlaylistName  = T.Text
type PlaylistID    = T.Text
type PlaylistURI   = T.Text

data SpotTrack = SpotTrack {
    track_name :: TrackName
  , track_id   :: TrackID
  , track_uri  :: TrackURI  
} deriving (Show, Generic)

data SpotArtist = SpotArtist {
    artist_name :: ArtistName
  , artist_id   :: ArtistID
  , artist_uri  :: ArtistURI  
} deriving (Show, Generic)

data SpotAlbum = SpotAlbum {
    album_name :: AlbumName
  , album_id   :: AlbumID
  , album_uri  :: AlbumURI  
} deriving (Show, Generic)

data SpotPlaylist = SpotPlaylist {
    playlist_name :: PlaylistName
  , playlist_id   :: PlaylistID
  , playlist_uri  :: PlaylistURI  
} deriving (Show, Generic)

data UriList = UriList {
  uris :: [T.Text]  
} deriving (Show, Generic)

instance FromJSON UriList
instance ToJSON   UriList



----------------------------------------
-- Lenses for common things to access


getItemNames :: AsValue body0 => Response body0 -> [T.Text]
getItemNames r =  r ^.. responseBody . key "items" . values . key "name" . _String

getItemIds :: AsValue body0 => Response body0 -> [T.Text]
getItemIds r =  r ^.. responseBody . key "items" . values . key "id" . _String

getItemUris :: AsValue body0 => Response body0 -> [T.Text]
getItemUris r =  r ^.. responseBody . key "items" . values . key "uri" . _String

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


----------------------------------------
-- Add the items into Neo4j database





----------------------------------------
-- https://developer.spotify.com/web-api/authorization-guide/
-- auth code stuff


----------------------------------------
-- REGEX THINGS

extractNameArtist :: (a, [T.Text]) -> Maybe (TrackName, ArtistName)
extractNameArtist (_, [track_name, artist_name]) = Just (track_name, artist_name)
extractNameArtist (_, _) = Nothing

getExplicitParodies :: [TrackName] -> [TrackName]
getExplicitParodies  = filter (\x -> length (scan parodyRegex x) > 0)
  
-- | Given a list of tuples where the second element is a Maybe type,
-- filter out the Nothing values.
filterSndNothing :: (IsSequence seq, Element seq ~ (t, Maybe a)) => seq -> seq
filterSndNothing = filter (\(_, m) -> isJust m)


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





----------------------------------------
  
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
  let al_parodies_by_regexp   = filter (\x -> length (scan parodyRegex (track_name x)) > 0) al_tracks
  let p_extract = ( fromJust .  (fmap extractNameArtist) .    headMay .   (scan parodyRegex) . track_name)
  let parody_and_original =  map (\(a,b) -> (a, fromJust b))  $ filterSndNothing $ zip al_parodies_by_regexp (fmap p_extract al_parodies_by_regexp)
  parodied_tracks <- mapM ( (searchForTrack oab) . snd) parody_and_original

  
  -- Finally, zip together the al_parodies_by_regexp and parodied_tracks, taking care to remove the nothing values.
  let al_and_original =  map (\(a,b) -> (a, fromJust b))  $ filterSndNothing $ zip al_parodies_by_regexp parodied_tracks


  -- Create a playlist and add 

  user_id <- getMyUserID spot_auth

  putStrLn "done"

  

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- CODE SCRAPS
  -- songs_ids <- mapM (getSongsForAlbumID spot_auth) album_ids
  -- let song_id_list = concat songs_ids

  -- let parody_song_id_list = filter (\(sn, _ ) ->  length (scan parodyRegex sn) > 0) song_id_list

  -- ----------------------------------------
  -- -- for each parody, run the regex

  -- -- let parody_name_artist   = map (head . extractNameArtist . (scan parodyRegex) . snd) parody_song_id_list
  -- let regexed_parody_names = fmap  (  headMay .  (scan parodyRegex) . fst) parody_song_id_list
  -- let extracted            = catMaybes $  (fmap . fmap) extractNameArtist regexed_parody_names
  -- let matched_parodies     = filterSndNothing $ zip parody_song_id_list extracted
  
  -- matched_tracks <- mapM (searchForTrack oab) (map (fromJust . snd) matched_parodies)
  


  -- let new_pl_name = "sjb_parody_match_v0"
  -- makePlaylist spot_auth new_pl_name
