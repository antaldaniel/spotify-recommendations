
get_artist_audio_features <- function (artist = NULL,
                                       include_groups = "album",
                                       return_closest_artist = TRUE,
                                       dedupe_albums = TRUE,
                                       authorization = get_spotify_access_token()) {
  if (is_uri(artist)) {
    artist_info <- get_artist(artist, authorization = authorization)
    artist_id <- artist_info$id
    artist_name <- artist_info$name
  }  else {
    artist_ids <- search_spotify(artist, "artist",
                                 authorization = authorization)
    if (return_closest_artist) {
      artist_id <- artist_ids$id[1]
      artist_name <- artist_ids$name[1]
    }
    else {
      choices <- map_chr(1:length(artist_ids$name), function(x) {
        str_glue("[{x}] {artist_ids$name[x]}")
      }) %>% paste0(collapse = "\n\t")
      cat(str_glue("We found the following artists on Spotify matching \"{artist}\":\n\n\t{choices}\n\nPlease type the number corresponding to the artist you're interested in."),
          sep = "")
      selection <- as.numeric(readline())
      artist_id <- artist_ids$id[selection]
      artist_name <- artist_ids$name[selection]
    }
  }

  artist_albums <- spotifyr::get_artist_albums(
    artist_id,
    include_groups = include_groups,
    include_meta_info = TRUE,
    authorization = authorization)

  artist_albums$items

  num_loops_artist_albums <- ceiling(artist_albums$total/20)

  if (num_loops_artist_albums > 1) {
    artist_albums <- map_df(1:num_loops_artist_albums, function(this_loop) {
      get_artist_albums(artist_id,
                        include_groups = include_groups,
                        offset = (this_loop - 1) * 20,
                        authorization = authorization)
    })
  } else if ( length(artist_albums$items)==0 ) {

    names(artist_albums)


    artist_albums <- get_artist_top_tracks(artist_id, authorization = authorization)

    names ( artist_albums)
    artist_albums <- purrr::set_names ( x = artist_albums,
                                        gsub("album\\.release", "album_release", names(artist_albums)))


  } else {
    artist_albums <- artist_albums$items
  }

  if ( !any( c("album.id", "album.name") %in% names(artist_albums)) ) {

    ## This is the base case
    artist_albums <- artist_albums %>%
      rename(album.id   = .data$id,
             album.name = .data$name )
  }

  if ( "album_release_date" %in% names(artist_albums) ) {

    ## This is when there were no albums and top tracks were used
    artist_albums <- artist_albums %>%
      rename(album.release_date   = .data$album_release_date)
  } else if ( ! "album.release_date" %in% names(artist_albums) ) {
    artist_albums <- artist_albums %>%
      rename(album.release_date   = .data$release_date)
  }

  artist_albums <- artist_albums %>%
    mutate(album.release_year = as.numeric(
      as.character(
        substr(.data$album.release_date, 1,4)
        )
      ))

  if (dedupe_albums) {
    artist_albums <- dedupe_album_names(df = artist_albums,
                                        album_name_col = 'album.name',
                                        album_release_year_col = 'album.release_year')
  }


  if ( "album.id" %in% names(artist_albums) )  {
    artist_albums <- artist_albums %>%
      dplyr::rename ( album_id = album.id )
  }

  album_tracks <- map_df(artist_albums$album_id, function(this_album_id) {
    album_tracks <- get_album_tracks(this_album_id,
                                     include_meta_info = TRUE,
                                     authorization = authorization)
    num_loops_album_tracks <- ceiling(album_tracks$total/20)

    if (num_loops_album_tracks > 1) {
      album_tracks <- map_df(1:num_loops_album_tracks,
                             function(this_loop) {
                               get_album_tracks(this_album_id, offset = (this_loop -
                                                                           1) * 20, authorization = authorization)
                             })
    }
    else {
      album_tracks <- album_tracks$items
    }
    album_tracks <- album_tracks %>%
      mutate(album_id = this_album_id,
             album_name = artist_albums$album_name[artist_albums$album_id ==
                                                     this_album_id]) %>% rename(track_name = name,
                                                                                track_uri = uri,
                                                                                track_preview_url = preview_url,
                                                                                track_href = href,
                                                                                track_id = id)
  })

  dupe_columns <- c("duration_ms", "type", "uri",
                    "track_href")

  num_loops_tracks <- ceiling(nrow(album_tracks)/100)

  track_audio_features <- map_df(1:num_loops_tracks, function(this_loop) {
    track_ids <- album_tracks %>%
      slice(((this_loop * 100) -
               99):(this_loop * 100)) %>%
      pull(track_id)
    get_track_audio_features(track_ids, authorization = authorization)
  })

  track_audio_features <- track_audio_features %>%
    select( #-.data$dupe_columns
       -all_of(dupe_columns)
       ) %>%
    rename(track_id = id) %>%
    left_join(album_tracks, by = "track_id")

  names ( artist_albums)

  if ( "album.release_date" %in% names (artist_albums) ) {
    artist_albums <- artist_albums %>%
      rename ( album_release_date = album.release_date)
  }

  if ( "album.release_year" %in% names (artist_albums) ) {
    artist_albums <- artist_albums %>%
      rename ( album_release_year = album.release_year)
  }

  if ( "release_date" %in% names (artist_albums) ) {
    artist_albums <- artist_albums %>%
      rename ( album_release_year = release_date )
  }

  artist_albums %>%
    mutate(artist_name = artist_name,
           artist_id = artist_id) %>%
    select(artist_name, artist_id, album_id, album_type,
           album_images = images,
           album_release_date, # = release_date,
           album_release_year,
           album_release_date_precision = release_date_precision) %>%
    left_join(track_audio_features, by = "album_id") %>%
    mutate(key_name = pitch_class_lookup[key + 1], mode_name = case_when(mode ==
                                                                           1 ~ "major", mode == 0 ~ "minor", TRUE ~
                                                                           as.character(NA)), key_mode = paste(key_name, mode_name))
}
