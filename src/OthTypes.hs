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
-- Other types for the parody bot. 
----------------------------------------------------------------------------

module OthTypes where

import ClassyPrelude
import Data.Hashable
import Data.Aeson --  (toJSON, pairs, (.=))
import Data.Aeson.Lens

import Data.Text as T
----------------------------------------


