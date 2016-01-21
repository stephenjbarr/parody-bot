{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables, NoImplicitPrelude, QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches #-}

module Main where
import ClassyPrelude
import Network.Wreq
import Control.Lens
import Data.Aeson (toJSON)
import Data.Aeson.Lens
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import Text.Regex.PCRE.Heavy as PH (Regex, re, scan)

-- import qualified SpotTypes as S
import MyRegexes


--------------------------------------------------------------------------------
-- A few newtypes so that I am not keeping track of "what Text means what?"

-- newtype TrackName  = TrackName  T.Text
-- newtype TrackID    = TrackID    T.Text
-- newtype TrackURI   = TrackURI   T.Text


-- newtype ArtistName = ArtistName T.Text
-- newtype ArtistID   = ArtistID   T.Text
-- newtype ArtistURI  = ArtistURI  T.Text

-- newtype AlbumName  = AlbumName  T.Text
-- newtype AlbumID    = AlbumID    T.Text
-- newtype AlbumURI   = AlbumURI   T.Text

type TrackName  = T.Text
type TrackID    = T.Text
type TrackURI   = T.Text


type ArtistName = T.Text
type ArtistID   = T.Text
type ArtistURI  = T.Text

type AlbumName  = T.Text
type AlbumID    = T.Text
type AlbumURI   = T.Text





----------------------------------------
-- Bits of data for accessing the Spotify web service.
-- This should be in a config file somewhere
spot_endpt :: String
spot_endpt = "https://api.spotify.com/v1/"

spot_cli_id     :: Text     = "ed8c0a8b345b41c59793a92f7a1984ee"
spot_cli_secret :: Text = "93f44712e4414dee8182a98cc1c6b184"
spot_req_sec :: Text    = spot_cli_id ++  ":" ++ spot_cli_secret


----------------------------------------
-- Processing the album Names


getAlbumNames :: AsValue body0 => Response body0 -> [AlbumName]
getAlbumNames r =  r ^.. responseBody . key "items" . values . key "name" . _String

getAlbumIds :: AsValue body0 => Response body0 -> [AlbumID]
getAlbumIds r =  r ^.. responseBody . key "items" . values . key "id" . _String


getSongsForAlbumID :: Options -> AlbumID -> IO [(TrackName, TrackID)]
getSongsForAlbumID xopt album_id = do
  let endpt = spot_endpt ++ "albums/" ++ (T.unpack album_id) ++ "/tracks?offset=0&limit=50&market=US"
  r <- getWith xopt endpt
  let track_names =  r ^.. responseBody . key "items" . values . key "name" . _String
  let track_ids   =  r ^.. responseBody . key "items" . values . key "id" . _String
  return $ zip track_names track_ids


-- hasParodyStringInTitle :: T.Text -> Bool 
-- hasParodyStringInTitle x = 


extractNameArtist (_, [song_name, artist_name]) = Just (song_name, artist_name)
extractNameArtist (_, _) = Nothing


getExplicitParodies :: [TrackName] -> [TrackName]
getExplicitParodies  = filter (\x -> length (scan parodyRegex x) > 0)
  


    
----------------------------------------

searchForTrackOpts :: Auth -> (TrackName, ArtistName) -> Options
searchForTrackOpts oab (song_name, artist_name) = defaults & param "type" .~ ["track"] & param "q" .~ [song_name ++ " " ++ artist_name] & auth ?~ oab

searchForTrack :: Auth ->  (TrackName, ArtistName) -> IO (Maybe (TrackURI, TrackName))
-- searchForTrack = error "undefined"
searchForTrack oab (song_name, artist_name) = do
  let my_opts = searchForTrackOpts oab (song_name, artist_name)
  let endpt   = spot_endpt ++ "search" 
  rn <- getWith my_opts endpt
  let rid   = rn ^.. responseBody . key "tracks" . key "items" . values . key "uri" . _String
  let rname = rn ^.. responseBody . key "tracks" . key "items" . values . key "name" . _String
  return $ headMay $ zip rid rname



----------------------------------------


-- | Given a list of tuples where the second element is a Maybe type,
-- filter out the Nothing values.
filterSndNothing :: (IsSequence seq, Element seq ~ (t, Maybe a)) => seq -> seq
filterSndNothing = filter (\(_, m) -> isJust m)


main :: IO ()
main = do
  putStrLn "hello world"

  let sreq =  decodeUtf8 $  B64.encode $ encodeUtf8 spot_req_sec
  let bearer = "Basic " ++ (T.unpack sreq)
  let auth_opts = defaults & header "Authorization" .~ [encodeUtf8 (T.pack bearer)]

  auth_req <- postWith auth_opts "https://accounts.spotify.com/api/token" ["grant_type" := ("client_credentials" :: T.Text)]
  let access_token = auth_req ^. responseBody . key "access_token" . _String

  let oab = oauth2Bearer $ encodeUtf8 access_token
  let spot_auth = defaults & auth ?~ oab

  r <- getWith spot_auth (spot_endpt ++ "artists/1bDWGdIC2hardyt55nlQgG/albums") 

  let album_names = getAlbumNames r
  let album_ids   = getAlbumIds   r

  songs_ids <- mapM (getSongsForAlbumID spot_auth) album_ids
  let song_id_list = concat songs_ids

  let parody_song_id_list = filter (\(sn, _ ) ->  length (scan parodyRegex sn) > 0) song_id_list

  ----------------------------------------
  -- for each parody, run the regex

  -- let parody_name_artist   = map (head . extractNameArtist . (scan parodyRegex) . snd) parody_song_id_list
  let regexed_parody_names = fmap  (  headMay .  (scan parodyRegex) . fst) parody_song_id_list
  let extracted            = catMaybes $  (fmap . fmap) extractNameArtist regexed_parody_names
  let matched_parodies     = filterSndNothing $ zip parody_song_id_list extracted
  
  matched_tracks <- mapM (searchForTrack oab) (map (fromJust . snd) matched_parodies)
  
  putStrLn "done"

  

  -- shit
  -- r ^.. responseBody . key "items" . values . key "name" . _String
  -- r2 <- getWith spot_opts ("https://api.spotify.com/v1/artists/1bDWGdIC2hardyt55nlQgG/albums?offset=20&limit=20&album_type=single,album,compilation,appears_on,ep")

