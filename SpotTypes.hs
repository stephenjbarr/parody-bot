{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}

module SpotTypes where

import           System.Exit        (exitFailure, exitSuccess)
import           System.IO          (stderr, hPutStrLn)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           System.Environment (getArgs)
import           Control.Monad      (forM_, mzero, join)
import           Control.Applicative
import           Data.Aeson.AutoType.Alternative
import           Data.Aeson(decode, Value(..), FromJSON(..), ToJSON(..),
                            (.:), (.:?), (.=), object)
import           Data.Text (Text)
import           GHC.Generics

-- | Workaround for https://github.com/bos/aeson/issues/287.
o .:?? val = fmap join (o .:? val)


data ImagesElt = ImagesElt { 
    imagesEltHeight :: Int,
    imagesEltUrl :: Text,
    imagesEltWidth :: Int
  } deriving (Show,Eq,Generic)


instance FromJSON ImagesElt where
  parseJSON (Object v) = ImagesElt <$> v .:   "height" <*> v .:   "url" <*> v .:   "width"
  parseJSON _          = mzero


instance ToJSON ImagesElt where
  toJSON (ImagesElt {..}) = object ["height" .= imagesEltHeight, "url" .= imagesEltUrl, "width" .= imagesEltWidth]


data ExternalUrls = ExternalUrls { 
    externalUrlsSpotify :: Text
  } deriving (Show,Eq,Generic)


instance FromJSON ExternalUrls where
  parseJSON (Object v) = ExternalUrls <$> v .:   "spotify"
  parseJSON _          = mzero


instance ToJSON ExternalUrls where
  toJSON (ExternalUrls {..}) = object ["spotify" .= externalUrlsSpotify]


data ArtistsElt = ArtistsElt { 
    artistsEltExternalUrls :: ExternalUrls,
    artistsEltUri :: Text,
    artistsEltHref :: Text,
    artistsEltName :: Text,
    artistsEltId :: Text,
    artistsEltType :: Text
  } deriving (Show,Eq,Generic)


instance FromJSON ArtistsElt where
  parseJSON (Object v) = ArtistsElt <$> v .:   "external_urls" <*> v .:   "uri" <*> v .:   "href" <*> v .:   "name" <*> v .:   "id" <*> v .:   "type"
  parseJSON _          = mzero


instance ToJSON ArtistsElt where
  toJSON (ArtistsElt {..}) = object ["external_urls" .= artistsEltExternalUrls, "uri" .= artistsEltUri, "href" .= artistsEltHref, "name" .= artistsEltName, "id" .= artistsEltId, "type" .= artistsEltType]


data Followers = Followers { 
    followersHref :: (Maybe Value),
    followersTotal :: Int
  } deriving (Show,Eq,Generic)


instance FromJSON Followers where
  parseJSON (Object v) = Followers <$> v .:?? "href" <*> v .:   "total"
  parseJSON _          = mzero


instance ToJSON Followers where
  toJSON (Followers {..}) = object ["href" .= followersHref, "total" .= followersTotal]


data ItemsElt = ItemsElt { 
    itemsEltImages :: (Maybe ([ImagesElt:|:[(Maybe Value)]])),
    itemsEltExternalUrls :: ExternalUrls:|:[(Maybe Value)],
    itemsEltAlbumType :: (Maybe (Text:|:[(Maybe Value)])),
    itemsEltPreviewUrl :: (Maybe (Text:|:[(Maybe Value)])),
    itemsEltUri :: Text,
    itemsEltExplicit :: (Maybe (Bool:|:[(Maybe Value)])),
    itemsEltDiscNumber :: (Maybe (Int:|:[(Maybe Value)])),
    itemsEltHref :: Text,
    itemsEltPopularity :: (Maybe (Int:|:[(Maybe Value)])),
    itemsEltDurationMs :: (Maybe (Int:|:[(Maybe Value)])),
    itemsEltName :: Text,
    itemsEltArtists :: (Maybe ([ArtistsElt])),
    itemsEltFollowers :: (Maybe (Followers:|:[(Maybe Value)])),
    itemsEltId :: Text,
    itemsEltGenres :: (Maybe ([Text:|:[(Maybe Value)]])),
    itemsEltType :: Text,
    itemsEltTrackNumber :: (Maybe (Int:|:[(Maybe Value)])),
    itemsEltAvailableMarkets :: (Maybe ([Text]))
  } deriving (Show,Eq,Generic)


instance FromJSON ItemsElt where
  parseJSON (Object v) = ItemsElt <$> v .:?? "images" <*> v .:   "external_urls" <*> v .:?? "album_type" <*> v .:?? "preview_url" <*> v .:   "uri" <*> v .:?? "explicit" <*> v .:?? "disc_number" <*> v .:   "href" <*> v .:?? "popularity" <*> v .:?? "duration_ms" <*> v .:   "name" <*> v .:?? "artists" <*> v .:?? "followers" <*> v .:   "id" <*> v .:?? "genres" <*> v .:   "type" <*> v .:?? "track_number" <*> v .:?? "available_markets"
  parseJSON _          = mzero


instance ToJSON ItemsElt where
  toJSON (ItemsElt {..}) = object ["images" .= itemsEltImages, "external_urls" .= itemsEltExternalUrls, "album_type" .= itemsEltAlbumType, "preview_url" .= itemsEltPreviewUrl, "uri" .= itemsEltUri, "explicit" .= itemsEltExplicit, "disc_number" .= itemsEltDiscNumber, "href" .= itemsEltHref, "popularity" .= itemsEltPopularity, "duration_ms" .= itemsEltDurationMs, "name" .= itemsEltName, "artists" .= itemsEltArtists, "followers" .= itemsEltFollowers, "id" .= itemsEltId, "genres" .= itemsEltGenres, "type" .= itemsEltType, "track_number" .= itemsEltTrackNumber, "available_markets" .= itemsEltAvailableMarkets]


data Artists = Artists { 
    artistsNext :: (Maybe Value),
    artistsOffset :: Int,
    artistsItems :: [ItemsElt],
    artistsHref :: Text,
    artistsTotal :: Int,
    artistsLimit :: Int,
    artistsPrevious :: (Maybe Value)
  } deriving (Show,Eq,Generic)


instance FromJSON Artists where
  parseJSON (Object v) = Artists <$> v .:?? "next" <*> v .:   "offset" <*> v .:   "items" <*> v .:   "href" <*> v .:   "total" <*> v .:   "limit" <*> v .:?? "previous"
  parseJSON _          = mzero


instance ToJSON Artists where
  toJSON (Artists {..}) = object ["next" .= artistsNext, "offset" .= artistsOffset, "items" .= artistsItems, "href" .= artistsHref, "total" .= artistsTotal, "limit" .= artistsLimit, "previous" .= artistsPrevious]


data TopLevel = TopLevel { 
    topLevelNext :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelOffset :: (Maybe (Int:|:[(Maybe Value)])),
    topLevelItems :: (Maybe ([ItemsElt])),
    topLevelHref :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelTotal :: (Maybe (Int:|:[(Maybe Value)])),
    topLevelArtists :: (Maybe (Artists:|:[(Maybe Value)])),
    topLevelLimit :: (Maybe (Int:|:[(Maybe Value)])),
    topLevelPrevious :: (Maybe Value)
  } deriving (Show,Eq,Generic)


instance FromJSON TopLevel where
  parseJSON (Object v) = TopLevel <$> v .:?? "next" <*> v .:?? "offset" <*> v .:?? "items" <*> v .:?? "href" <*> v .:?? "total" <*> v .:?? "artists" <*> v .:?? "limit" <*> v .:?? "previous"
  parseJSON _          = mzero


instance ToJSON TopLevel where
  toJSON (TopLevel {..}) = object ["next" .= topLevelNext, "offset" .= topLevelOffset, "items" .= topLevelItems, "href" .= topLevelHref, "total" .= topLevelTotal, "artists" .= topLevelArtists, "limit" .= topLevelLimit, "previous" .= topLevelPrevious]




parse :: FilePath -> IO TopLevel
parse filename = do input <- BSL.readFile filename
                    case decode input of
                      Nothing -> fatal $ case (decode input :: Maybe Value) of
                                           Nothing -> "Invalid JSON file: "     ++ filename
                                           Just v  -> "Mismatched JSON value from file: " ++ filename
                      Just r  -> return (r :: TopLevel)
  where
    fatal :: String -> IO a
    fatal msg = do hPutStrLn stderr msg
                   exitFailure

main :: IO ()
main = do
  filenames <- getArgs
  forM_ filenames (\f -> parse f >>= (\p -> p `seq` putStrLn $ "Successfully parsed " ++ f))
  exitSuccess


