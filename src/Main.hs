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

spot_endpt :: String
spot_endpt = "https://api.spotify.com/v1/"

spot_cli_id :: Text     = "ed8c0a8b345b41c59793a92f7a1984ee"
spot_cli_secret :: Text = "93f44712e4414dee8182a98cc1c6b184"
spot_req_sec :: Text    = spot_cli_id ++  ":" ++ spot_cli_secret

spot_opts = defaults & auth ?~ oauth2Bearer "BQCYVfLRUSrpn6LcOYxwVpJsZHyeGQZL-xbgH1QX7FEm9WyzFS7LG22uDz4UpjrA2Iy__qPUXifiw5sOomCgpg"


getAlbumNames :: AsValue body0 => Response body0 -> [T.Text]
getAlbumNames r = r ^.. responseBody . key "items" . values . key "name" . _String

getAlbumIds :: AsValue body0 => Response body0 -> [T.Text]
getAlbumIds r = r ^.. responseBody . key "items" . values . key "id" . _String

-- getNext r = r ^? responseBody . key "next" . _String

getSongsForAlbumID :: Options -> T.Text -> IO [(T.Text, T.Text)]
getSongsForAlbumID xopt album_id = do
  let endpt = spot_endpt ++ "albums/" ++ (T.unpack album_id) ++ "/tracks?offset=0&limit=50&market=US"
  r <- getWith xopt endpt
  let track_names = r ^.. responseBody . key "items" . values . key "name" . _String
  let track_ids   = r ^.. responseBody . key "items" . values . key "id" . _String
  return $ zip track_names track_ids


-- hasParodyStringInTitle :: T.Text -> Bool 
-- hasParodyStringInTitle x = 


extractNameArtist (_, [song_name, artist_name]) = Just (song_name, artist_name)
extractNameArtist (_, _) = Nothing


getExplicitParodies :: [T.Text] -> [T.Text]
getExplicitParodies  = filter (\x -> length (scan parodyRegex x) > 0)
  


    
----------------------------------------

searchForTrackOpts :: Auth -> (T.Text, T.Text) -> Options
searchForTrackOpts oab (song_name, artist_name) = defaults & param "type" .~ ["track"] & param "q" .~ [song_name ++ " " ++ artist_name] & auth ?~ oab

searchForTrack :: Auth ->  (T.Text, T.Text) -> IO (Maybe (T.Text, T.Text))
-- searchForTrack = error "undefined"
searchForTrack oab (song_name, artist_name) = do
  let my_opts = searchForTrackOpts oab (song_name, artist_name)
  let endpt   = spot_endpt ++ "search" 
  rn <- getWith my_opts endpt
  let rid   = rn ^.. responseBody . key "tracks" . key "items" . values . key "uri" . _String
  let rname = rn ^.. responseBody . key "tracks" . key "items" . values . key "name" . _String
  return $ headMay $ zip rid rname

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

  let parody_song_id_list = filter (\(sn, ) ->  length (scan parodyRegex sn) > 0) song_id_list

  ----------------------------------------
  -- for each parody, run the regex




  putStrLn "done"

  

  -- shit
  -- r ^.. responseBody . key "items" . values . key "name" . _String
  -- r2 <- getWith spot_opts ("https://api.spotify.com/v1/artists/1bDWGdIC2hardyt55nlQgG/albums?offset=20&limit=20&album_type=single,album,compilation,appears_on,ep")

