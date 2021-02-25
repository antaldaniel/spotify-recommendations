#' Get Local Recommendations
#'
#' This is the wrapper function to the Listen Local Demo recommendation system.
#'
#' @rdname recparam
#' @param recommendation_type Defaults to \code{'artists'}, can be \code{'release'}
#' or \code{'both'}.
#' @param limit Number of playlist items used for recommendation seed.
#' @param silent Defaults to \code{TRUE}.
##' @param authorization Defaults to \code{NULL} when
#' \code{\link[spotifyr]{get_spotify_access_token}} is invoked.
#' @param listen_local_artist_data If \code{NULL}, the functions try to load the data from the
#' spotifyrecommendations package.
#' @param local_genre_table_path If \code{NULL}, the functions try to load the data from the
#' spotifyrecommendations package.
#' @param artist_distance_data If \code{NULL}, the functions try to load the data from the
#' spotifyrecommendations package.
#' @param artist_genre_table_path If \code{NULL}, the functions try to load the data from the
#' spotifyrecommendations package.
#' @importFrom dplyr bind_rows ungroup filter select sample_n
#' @importFrom tidyselect all_of any_of
#' @importFrom spotifyr get_spotify_access_token get_artist_audio_features
#' @importFrom assertthat assert_that
#' @return A tibble of recommendations.
#' @export

get_local_recommendations <- function(
    playlist_id = "6KHw5aZWWsmRqpT7o290Mo",
    target_nationality = "sk",
    target_release = NULL,
    recommendation_type = "artists",
    limit = 20,
    n_rec = 4,
    silent = TRUE,
    authorization = NULL,
    listen_local_artist_data = 'not_included/listen_local_artists.rds',
    artist_distance_data = 'not_included/artist_distances.rds',
    local_genre_table_path = "not_included/local_genre_table.rds",
    artist_genre_table_path = "not_included/artist_genre_table.rds") {

  if (is.null(authorization)) {
    authorization <- spotifyr::get_spotify_access_token()
  }

  ## Load target artist data ---------------------------------------

  if ( ! exists ("listen_local_artist_data") ) {
    # In the shiny app, the object must exist in the global session.
    if ( !is.null(listen_local_artist_data) ) {
      load_data ( variable_name = "listen_local_artists",
                  data_path = listen_local_artist_data,
                  envir = environment() #default, not necessary parameter
      )
    } else {
      data("listen_local_artists", package = 'spotifyrecommendations', envir = environment())
    }
  }

  ## Get info about the user's playlist items from Spotify -----------

  user_playlist_info <- get_playlist_information(
    playlist_id  = playlist_id )

  if ( recommendation_type == 'artists' ) {
    target_nationality <- target_nationality
    target_release     <- NULL
  } else if ( recommendation_type == "release") {
    target_nationality <- NULL
    target_release     <- "sk"
  } else {
    if ( target_nationality != target_release ) {
      target_release     <- target_nationality
    }
  }

  vars_to_select <- c( "id", "name", "popularity",
                       "uri", "external_ids.isrc",
                       "release_country_code",
                       "target_artists" )

  target_artist_ids <- get_national_artist_ids(target_nationality)
  users_artists <- unique(user_playlist_info$user_playlist_artists$id)

  any ( users_artists %in% target_artist_ids )

  message ( "Getting initial user recommendations")

  initial_user_recommendations <- initial_recommendations(
    playlist_information = user_playlist_info,
    target_ids = target_artist_ids,
    limit = limit,
    silent = silent,
    authorization = authorization )

  local_recommendations <- initial_user_recommendations

  if (
    # recommendations are not based on artists
    !is.null(target_nationality) ) {
    local_recommendations <- local_recommendations %>%
      filter ( target_nationality == TRUE)
  }

  if (
    # release country does not play a role
    !is.null(target_release)
    ) {
    local_recommendations <- local_recommendations %>%
      filter ( target_release == target_release )
  }

  local_recommendations <- local_recommendations %>%
    select ( any_of(vars_to_select) )

  if ( nrow(local_recommendations)>=n_rec ) {  #should return n_rec number recommendations
    return(local_recommendations)
  }

  ### ----- artist based recommendations ---------------------------------

  message ( "... not sufficient, moving on to artits-based recommendations.")

  if ( !exists ( "artist_distances" ) ) {
    # Should exist in the global session of the Shiny app
    if ( !is.null(artist_distance_data) ) {
      load_data ( "artist_distances", artist_distance_data )
    } else {
      data("artist_distance_data", package = 'spotifyrecommendations', envir = environment())
    }
  }
  message ( paste0("There are ", nrow(artist_distances), " artist distances defined.") )

  ## recommend artists first
  recommended_by_artist_similarity <- get_local_artist_recommendations(
    user_playlist_artists = users_artists,
    target_ids = target_artist_ids,
    n_rec = n_rec,
    authorization = authorization
  )

  message ( paste0(length (recommended_by_artist_similarity), " similar artists found."))

  assertthat::assert_that(all ( recommended_by_artist_similarity %in% target_artist_ids ),
                          msg = "Wrong nationality is recommended.")

  tracks_by_artist_similarity <- lapply ( recommended_by_artist_similarity,
                     purrr::possibly(get_artist_audio_features, NULL))

  tracks_by_artist_similarity <- do.call( rbind, tracks_by_artist_similarity  )

  message ( paste0 (nrow ( tracks_by_artist_similarity ), " track candidates are available from the artists."))

  ## Find the most suitable tracks from the candidate list.
 local_recommendations_by_artist <- get_nearest_tracks(
    user_tracks = user_playlist_info$user_playlist_tracks,
    new_tracks  = tracks_by_artist_similarity )

  message ( "Nearest candidates are identified")

  if ( !is.null(local_recommendations_by_artist) ) {
    local_recommendations_2 <- local_recommendations_by_artist %>%
      mutate ( release_country_code = get_release_country(.data$external_ids.isrc),
               target_artists = TRUE ) %>%
      select ( all_of (vars_to_select)) %>%
      bind_rows (local_recommendations) %>%
      ungroup()

    if ( nrow(local_recommendations_2)>=n_rec) {
      return(sample_n(local_recommendations_2,size = n_rec))
    }
  } else {
    local_recommendations_2 <- local_recommendations_by_artist
    }

  ### ----- genre based recommendations ----------------------------------

  message ( "... not sufficient, moving on to genre-based recommendations.")

  if ( !exists("artist_genre_table")) {
    if ( !is.null(artist_genre_table_path) ) {
      load_data ( "artist_genre_table", artist_genre_table_path )
    } else {
      data("artist_genre_table_path", package = 'spotifyrecommendations', envir = environment() )
    }
  }

  if ( !exists("local_genre_table")) {
    if ( !is.null(local_genre_table_path) ) {
      load_data ( "local_genre_table", local_genre_table_path )
    } else {
      data("local_genre_table_path", package = 'spotifyrecommendations',envir = environment() )
    }

  }

  user_artists_by_genre <- get_artist_genre(
    user_playlist_artists = user_playlist_info$user_playlist_artists
  )

  ## recommend artists by genre first
  recommended_by_genre <- get_artist_recommendations_genre(
    artists_by_genre = user_artists_by_genre,
    target_nationality = target_nationality
  )

  if ( ! all (  recommended_by_genre %in% target_artist_ids ) ) {
    stop("Wrong nationality is recommended.")
  }

  tracks_by_genre_similarity <- lapply (
    recommended_by_genre,
    purrr::possibly(get_artist_audio_features, NULL))

  tracks_by_genre_similarity  <- do.call( rbind, tracks_by_genre_similarity )

  local_recommendations_by_genre <- get_nearest_tracks(
    user_tracks = user_playlist_info$user_playlist_tracks,
    new_tracks = tracks_by_genre_similarity,
    n_rec = n_rec)

  if ( !is.null(local_recommendations_by_genre ) ) {
    ## if the genre based recommendations were successful, add them to the the
    ## existing candidates

    local_recommendations_3 <- local_recommendations_by_genre  %>%
      mutate ( release_country_code = get_release_country(.data$external_ids.isrc),
               target_artists = TRUE ) %>%
      select ( all_of (vars_to_select)) %>%
      bind_rows (local_recommendations_2) %>%
      ungroup()

    if ( nrow(local_recommendations_3)>=n_rec ) {
      ## if the candidates are more than required, randomly select n_rec
      return( sample_n(local_recommendations_3, size = n_rec) )
    }
  } else {
    ## if there were no genre-based recommendations, give whatever we have
    local_recommendations_2
  }

}
