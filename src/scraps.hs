trackToNode :: SpotTrack -> NodeBatchIdentifier
trackToNode = error "not definied yet"

artistToNode :: SpotArtist -> NodeBatchIdentifier
artistToNode = error "not definied yet"

albumToNode :: SpotAlbum -> NodeBatchIdentifier
albumToNode = error "not definied yet"

albumToNode :: SpotAlbum -> NodeBatchIdentifier
albumToNode = error "not definied yet"


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- CODE SCRAPS
  -- songs_ids <- mapM (getSongsForAlbumID spot_auth) album_ids
  -- let song_id_list = concat songs_ids

  -- let parody_song_id_list = filter (\(sn, _ ) ->  length (scan parodyRegex sn) > 0) song_id_list

  -- ----------------------------------------
  -- -- for each parody, run the regex

  -- -- let parody_name_artist   = map (head . extractNameArtist . (scan parodyRegex) . snd) parody_song_id_list
  -- let regexed_parody_names = fmap  (  headMay .  (scan parodyRegex) . fst) parody_song_id_list
  -- let extracted            = catMaybes $  (fmap . fmap) extractNameArtist regexed_parody_names
  -- let matched_parodies     = filterSndNothing $ zip parody_song_id_list extracted
  
  -- matched_tracks <- mapM (searchForTrack oab) (map (fromJust . snd) matched_parodies)
  


  -- let new_pl_name = "sjb_parody_match_v0"
  -- makePlaylist spot_auth new_pl_name

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Alchemy Entity scraps


  -- let r_entity_name = r ^.. responseBody . key "entities" . values . key "disambiguated" . key "name" . _String
  -- let r_relevance   = r ^.. responseBody . key "entities" . values . key "relevance" . _Double
  -- let r_sentiment   = r ^.. responseBody . key "entities" . values . key "sentiment" . key "type" . _String
  -- let r_type        = r ^.. responseBody . key "entities" . values . key "type" . _String

  -- let st_arys       = r ^.. responseBody . key "entities" . values . key "disambiguated" . key "subType" . _Array
  -- let r_subtypes    = map (^.. traverse . _String) st_arys

  -- -- website, dbpedia, freebase, yago
  -- let r_ld_ws = r ^.. responseBody . key "entities" . values . key "disambiguated" . key "website" . _String

  -- ----------------------------------------


  -- let r_entity_name   = r ^.. responseBody . key "entities" . values . key "disambiguated" . key "name" . _String

  -- let r_relevance   = r ^.. responseBody . key "entities" . values . key "relevance" . _Double
  -- let r_sentiment   = r ^.. responseBody . key "entities" . values . key "sentiment" . key "type" . _String
  -- let r_type        = r ^.. responseBody . key "entities" . values . key "type" . _String

  -- let st_arys       = r ^.. responseBody . key "entities" . values . key "disambiguated" . key "subType" . _Array
  -- let r_subtypes    = map (^.. traverse . _String) st_arys

  -- -- website, dbpedia, freebase, yago
  -- let r_ld_ws = r ^.. responseBody . key "entities" . values . key "disambiguated" . key "website" . _String




  


  
  -- let valid_objs = r ^.. responseBody . key "entities" . _Array . traversed . filtered (\v -> (isJust $ v ^? key "type")  && (isJust $ v ^? key "relevance" ) && (isJust $ v ^? key "sentiment") && (isJust $ v ^? key "count") && (isJust $ v ^? key "text") && (isJust $ v ^? key "disambiguated") && (isJust $ v ^? key "disambiguated" . key "name") && (isJust $ v ^? key "disambiguated" . key "subtype"))
