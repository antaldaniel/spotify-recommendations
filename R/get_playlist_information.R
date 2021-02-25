#' Get Playlist Information
#'
#'  Get unique tracks and unique artists from a playlist in a list.
#'
#' @param playlist_id A single Spotify playlist id
#' @param playlist A playlist querried by  \code{\link[spotifyr]{get_playlist}}.
#' @inheritParams get_local_recommendations
#' @importFrom purrr map_df
#' @importFrom spotifyr get_playlist_audio_features
#' @importFrom dplyr count group_by arrange bind_cols case_when
#' @return A list of unique tracks and unique artists from a playlist.
#' @examples
#' # Get a pre-saved playlist
#' get_playlist_information('sound_of_dutch_indie_playlist')
#' @export

get_playlist_information <- function( playlist_id = NULL,
                                      playlist = NULL,
                                      authorization = NULL) {

  if (is.null(authorization)) authorization <- get_spotify_access_token()

  if  ( !is.null(playlist_id) ) {

    user_playlist <- possibly_get_playlist(playlist_id = playlist_id,
                                           authorization = authorization )
    if ( is.null(user_playlist)) {
      warning("User playlist could not be read.")
      return(NULL)
    }

    playlist_id <- case_when (
      playlist_id == 'sound_of_dutch_indie_playlist' ~,
      TRUE ~ playlist_id
    )

    ## retrieving the audio features of the list
    user_playlist_features <- spotifyr::get_playlist_audio_features(
      playlist_uris = playlist_id,
      authorization = authorization )

  } else {
    user_playlist <- playlist
    user_playlist_features <- spotifyr::get_track_audio_features(
      ids = user_playlist$tracks$items$track.id,
      authorization = authorization)
    user_playlist_features <- bind_cols (user_playlist$tracks$items,
                                         user_playlist_features)
  }

  . <- id <- n <- NULL

  if ( is.null(user_playlist) ) {
    warning ("Could not get playlist")
    return(NULL)}

  get_playlist_info <- function (user_playlist) {
    user_playlist_2 <- user_playlist
    user_playlist_2$tracks <- NULL
    user_playlist_2$external_urls <- NULL
    user_playlist_2$images <- NULL
    user_playlist_2$owner <- NULL
    if ( !is.null(user_playlist_2$followers$total)) {
      user_playlist_2$followers <- as.numeric(user_playlist_2$followers$total)
    } else {
      user_playlist_2$followers <- NA_real_
    }
    user_playlist_2$display_name <- as.character(unlist(user_playlist$owner$display_name))
    user_playlist_2$uri <- user_playlist$external_urls$spotify
    tibble::tibble(
      playlist_name = user_playlist_2$name,
      description = user_playlist_2$description,
      spotify_playlist_id = user_playlist_2$id,
      followers = user_playlist_2$followers,
      url = user_playlist_2$href,
      retrieve_date = Sys.Date(),
      retrieve_time = Sys.time()
    )
  }


  user_playlist_features$release_country_code <- get_release_country(
    user_playlist_features$track.external_ids.isrc)

  user_playlist_artists <- do.call ( rbind,
                                     user_playlist_features$track.album.artists
                                     )

  unique_playlist_artists <-  user_playlist_artists  %>%
    group_by ( .data$id ) %>%
    count() %>%
    arrange ( .data$n )

  unique_playlist_tracks <- user_playlist_features

   list (
    user_playlist_info = get_playlist_info (user_playlist = user_playlist),
    user_playlist_tracks =  unique_playlist_tracks,
    user_playlist_artists = unique_playlist_artists
    )
}




