{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables, NoImplicitPrelude, QuasiQuotes, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches #-}


-----------------------------------------------------------------------------
-- |
-- Module      :  NeoParodyInterface
-- Copyright   :  (C) 2015-2016, Stephen J. Barr
-- License     :  None
-- Maintainer  :  Stephen J. Barr <stephen@planetbarr.com>
-- Stability   :  experimental
--
-- This is the interface between Neo4j and the Parody Bot.
----------------------------------------------------------------------------

module NeoParodyInterface where

import           ClassyPrelude
import           Database.Neo4j
import           Database.Neo4j.Graph as G
import           Database.Neo4j.Batch as B
import           Database.Neo4j.Types as NT
import qualified Database.Neo4j.Transactional.Cypher as TC
import           SpotTypes

--------------------------------------------------------------------------------


-- | For a SpotItem, create a NodeBatchIdentifier that can be inserted into the graph
-- as part of a batch operation. This is the function that should be exported, and
-- can use more specific helper functions if necessary.
-- 
itemToNode :: SpotItem -> NodeBatchIdentifier
itemToNode = error "ndy"






--------------------------------------------------------------------------------



--------------------------------------------------------------------------------

lookupItem :: SpotItem -> IO (Maybe Node)
lookupItem = error "ndy"

artistAuthoredTrack :: SpotArtist -> SpotAlbum -> IO Graph
artistAuthoredTrack = error "not defined yet"

addParodiedRelationship :: SpotTrack -> SpotTrack -> IO Graph
addParodiedRelationship = error "ndy"

addSameArticleRelationship :: SpotItem -> SpotItem  -> IO Graph
addSameArticleRelationship = error "ndy"
