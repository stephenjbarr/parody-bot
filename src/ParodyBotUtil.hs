{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables, NoImplicitPrelude, QuasiQuotes, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches #-}

module ParodyBotUtil where


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



----------------------------------------
-- REGEX THINGS

extractNameArtist :: (a, [T.Text]) -> Maybe (TrackName, ArtistName)
extractNameArtist (_, [track_name, artist_name]) = Just (track_name, artist_name)
extractNameArtist (_, _) = Nothing

getExplicitParodies :: [TrackName] -> [TrackName]
getExplicitParodies  = filter (\x -> length (scan parodyRegex x) > 0)
  
-- | Given a list of tuples where the second element is a Maybe type,
-- filter out the Nothing values.
-- filterSndNothing :: (IsSequence seq, Element seq ~ (t, Maybe a)) => seq -> seq
-- filterSndNothing :: [(a, Maybe b)] -> [(a,b)]
-- filterSndNothing = filter (\(x, m) -> isJust m)


filterListJustSnd :: [(a, Maybe b)] -> [Maybe (a,b)]
filterListJustSnd abl = map f0 abl
  where
    f0 (x,y) = case (x, y) of (c, Just d) -> Just (c, d)
                              _           -> Nothing

makePossibleDict :: (Eq a, Hashable a) => [(a, Maybe b)] -> HMS.HashMap a b
makePossibleDict abl = HMS.fromList $ catMaybes $ filterListJustSnd abl

combine2Tupe :: [(a,a)] -> [a]
combine2Tupe x = L.concat $ map (\(x,y) -> [x] ++ [y]) x
