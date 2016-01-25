{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables, NoImplicitPrelude, QuasiQuotes, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  AlchemyInterface
-- Copyright   :  (C) 2015-2016, Stephen J. Barr
-- License     :  None
-- Maintainer  :  Stephen J. Barr <stephen@planetbarr.com>
-- Stability   :  experimental
--
-- This is an interface to AlchemyAPI
----------------------------------------------------------------------------



import           ClassyPrelude
import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml as Y
import GHC.Generics
import Data.Aeson

data AlchemyApiCred = AlchemyApiCred { alchemy_api_key :: String } deriving (Show, Generic)
instance FromJSON AlchemyApiCred


getAlchemyCred :: FilePath -> IO AlchemyApiCred
getAlchemyCred fp = do
  content <- BS.readFile fp -- (4)
  let parsedContent = Y.decode content :: Maybe AlchemyApiCred -- (5)
  case parsedContent of
    Nothing             -> error "Could not parse config file."
    Just creds          -> return $ creds

----------------------------------------
-- READ THE API KEY FROM SETTINGS





-- Some static data 

-- http://gateway-a.watsonplatform.net/calls/url/URLGetRankedNamedEntities
