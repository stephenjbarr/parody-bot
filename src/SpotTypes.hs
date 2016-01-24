{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables, NoImplicitPrelude, QuasiQuotes, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches #-}


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

data UriList = UriList {
  uris :: [Text]  
} deriving (Show, Generic, Eq)

instance FromJSON UriList
instance ToJSON   UriList

instance Hashable SpotTrack
instance Hashable SpotArtist
instance Hashable SpotAlbum
instance Hashable SpotPlaylist
instance Hashable UriList
