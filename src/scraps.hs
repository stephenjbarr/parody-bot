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
