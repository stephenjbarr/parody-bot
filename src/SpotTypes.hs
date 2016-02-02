{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables, NoImplicitPrelude, QuasiQuotes, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches #-}


-----------------------------------------------------------------------------
-- |
-- Module      :  SpotTypes
-- Copyright   :  (C) 2015-2016, Stephen J. Barr
-- License     :  None
-- Maintainer  :  Stephen J. Barr <stephen@planetbarr.com>
-- Stability   :  experimental
--
-- Types for the ParodyBot, including some very simple access to Spotify.
----------------------------------------------------------------------------


module SpotTypes where



----------------------------------------

import ClassyPrelude
import Data.Hashable
import Data.Aeson --  (toJSON, pairs, (.=))
import Data.Aeson.Lens

import Data.Text as T
----------------------------------------

type UserID     = Text

type TrackName  = Text
type TrackID    = Text
type TrackURI   = Text

type ArtistName = Text
type ArtistID   = Text
type ArtistURI  = Text

type AlbumName  = Text
type AlbumID    = Text
type AlbumURI   = Text

type PlaylistName  = Text
type PlaylistID    = Text
type PlaylistURI   = Text

--------------------------------------------------------------------------------

data SpotTrack = SpotTrack {
    track_name :: TrackName
  , track_id   :: TrackID
  , track_uri  :: TrackURI  
} deriving (Show, Generic, Eq)


data SpotArtist = SpotArtist {
    artist_name :: ArtistName
  , artist_id   :: ArtistID
  , artist_uri  :: ArtistURI  
} deriving (Show, Generic, Eq)

data SpotAlbum = SpotAlbum {
    album_name :: AlbumName
  , album_id   :: AlbumID
  , album_uri  :: AlbumURI  
} deriving (Show, Generic, Eq)

data SpotPlaylist = SpotPlaylist {
    playlist_name :: PlaylistName
  , playlist_id   :: PlaylistID
  , playlist_uri  :: PlaylistURI  
} deriving (Show, Generic, Eq)


data SpotItem = 
    SPTrack    SpotTrack
  | SPArtist   SpotArtist
  | SPAlbum    SpotAlbum
  | SPPlaylist SpotPlaylist deriving (Show, Eq, Generic)


getSpotItemName :: SpotItem -> Text
getSpotItemName si = case si of
   SPTrack x    -> track_name  x
   SPArtist x   -> artist_name x
   SPAlbum x    -> album_name  x
   SPPlaylist x -> playlist_name x

extractSpotTrack :: SpotItem -> Maybe SpotTrack
extractSpotTrack si = case si of
  SPTrack x     -> Just x
  _             -> Nothing

extractSpotArtist :: SpotItem -> Maybe SpotArtist
extractSpotArtist si = case si of
  SPArtist x     -> Just x
  _             -> Nothing

extractSpotAlbum :: SpotItem -> Maybe SpotAlbum
extractSpotAlbum si = case si of
  SPAlbum x     -> Just x
  _             -> Nothing


extractSpotPlaylist :: SpotItem -> Maybe SpotPlaylist
extractSpotPlaylist si = case si of
  SPPlaylist x     -> Just x
  _             -> Nothing



--------------------------------------------------------------------------------

spotItemName :: SpotItem -> Text
spotItemName si = case si of
   SPTrack x    -> "Track"
   SPArtist x   -> "Artist"
   SPAlbum x    -> "Album"
   SPPlaylist x -> "Playlist"

--------------------------------------------------------------------------------


data UriList = UriList {
  uris :: [Text]  
} deriving (Show, Generic, Eq)

instance FromJSON UriList
instance ToJSON   UriList

instance Hashable SpotTrack
instance Hashable SpotArtist
instance Hashable SpotAlbum
instance Hashable SpotPlaylist
instance Hashable SpotItem
instance Hashable UriList
