{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}

module Artists where

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
    itemsEltImages :: [ImagesElt:|:[(Maybe Value)]],
    itemsEltExternalUrls :: ExternalUrls,
    itemsEltUri :: Text,
    itemsEltHref :: Text,
    itemsEltPopularity :: Int,
    itemsEltName :: Text,
    itemsEltFollowers :: Followers,
    itemsEltId :: Text,
    itemsEltGenres :: [Text:|:[(Maybe Value)]],
    itemsEltType :: Text
  } deriving (Show,Eq,Generic)


instance FromJSON ItemsElt where
  parseJSON (Object v) = ItemsElt <$> v .:   "images" <*> v .:   "external_urls" <*> v .:   "uri" <*> v .:   "href" <*> v .:   "popularity" <*> v .:   "name" <*> v .:   "followers" <*> v .:   "id" <*> v .:   "genres" <*> v .:   "type"
  parseJSON _          = mzero


instance ToJSON ItemsElt where
  toJSON (ItemsElt {..}) = object ["images" .= itemsEltImages, "external_urls" .= itemsEltExternalUrls, "uri" .= itemsEltUri, "href" .= itemsEltHref, "popularity" .= itemsEltPopularity, "name" .= itemsEltName, "followers" .= itemsEltFollowers, "id" .= itemsEltId, "genres" .= itemsEltGenres, "type" .= itemsEltType]


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
    topLevelArtists :: Artists
  } deriving (Show,Eq,Generic)


instance FromJSON TopLevel where
  parseJSON (Object v) = TopLevel <$> v .:   "artists"
  parseJSON _          = mzero


instance ToJSON TopLevel where
  toJSON (TopLevel {..}) = object ["artists" .= topLevelArtists]




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


