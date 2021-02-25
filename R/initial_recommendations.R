#' @title Get Initial Recommendations
#'
#' The initial recommendations are based on the Spotify algorithm, and try to
#' meet the user local content targets.
#'
#' @param playlist_information A list received from A playlist queried by
#' \code{{get_playlist_information}}.
#' @inheritParams get_local_recommendations
#' @rdname target_ids
#' @param limit How many items are used for the initial recommendation.
#' @importFrom dplyr bind_rows sample_n ungroup mutate bind_rows
#' @importFrom spotifyr get_recommendations_all
#' @importFrom purrr possibly
#' @import rlang
#' @return A tibble of recommendations. \code{recommendations_by_tracks$target_artists} contains a
#' logical vector, identifying the tracks that are among \code{target_ids}.
#' @export

initial_recommendations <- function( playlist_information,
                                     target_ids = NA_character_,
                                     limit = 20,
                                     silent = TRUE,
                                     authorization = NULL ) {

  if (is.null(authorization)) authorization <- get_spotify_access_token()

  ### Artist based recommendations -------------------------------------------------

  all_artists <- playlist_information$user_playlist_artists

  if ( length(all_artists$id) > limit ) {
    all_artists <- sample_n ( ungroup(all_artists),
                              size = limit )
  }

  ## Getting possible recommendations for all artists in the user playlist -------
  possible_recommendations_by_artists <- purrr::possibly(
    .f = get_recommendations_artists_all,
    otherwise = NULL)

  recommendations_by_artists <- possible_recommendations_by_artists (
    artist_ids = all_artists$id,
    authorization = authorization )

  if ( is.null(recommendations_by_artists) ) {
    warning ("No recommendation was made on the basis of original artists")
  }

  if ( length( playlist_information$user_playlist_tracks$track.id ) > limit ) {
    ## number of original user tracks are too numerous
    user_track_ids <- sample_n(
      ungroup( playlist_information$user_playlist_tracks), size = limit )
  } else {
    user_track_ids <-
      ungroup( playlist_information$user_playlist_tracks)
  }

  if ( !is.null(recommendations_by_artists) ) {

    recommendations_by_artists <- recommendations_by_artists %>%
      mutate (
        ## add release country information
        release_country_code = get_release_country( isrc = .data$external_ids.isrc )
        )

    rec_by_artists <- unlist (
         ## check if artists are among the targeted artist group ----
         vapply ( recommendations_by_artists$artists, function(x) {
           any ( x$id %in% target_ids) }, logical(1)
         )
         #sapply ( recommendations_by_artists$artists, function(x) {
         #   any ( x$id %in% target_ids) }
         # )
    )

    recommendations_by_artists$target_artists <- rec_by_artists

  } else {
    ## no initial recommendation was made
    if (!silent) {
      warning ("No recommendation was made on the basis of original artists.")
    }
  }

  ## Track based recommendations ------------------------------------------------------------------------
  ## otherwise similar to previous, but seed is tracks

  recommendations_by_tracks <- purrr::possibly(
    .f = spotifyr::get_recommendations_all,
    otherwise = NULL
  )(
    track_ids = unique (user_track_ids$track.id)
  )

  if ( ! is.null(recommendations_by_tracks) ) {
    recommendations_by_tracks <- recommendations_by_tracks %>%
      mutate ( release_country_code = get_release_country(
        isrc = .data$external_ids.isrc
      ))

    rec_by_tracks <- unlist (
      vapply ( recommendations_by_tracks$artists, function(x) {
        any ( x$id %in% target_ids) }, logical(1)
      )
    )

    recommendations_by_tracks$target_artists <- rec_by_tracks
  } else {
    if ( !silent) {
      warning ("No recommendation was made on the basis of original tracks.")

    }
  }

  ## Combination of recommendations --------------------------------
  ## Return whatever is possible
  if ( !is.null(recommendations_by_tracks) ) {
    if ( !is.null(recommendations_by_artists) ){
       dplyr::bind_rows ( recommendations_by_artists,
                          recommendations_by_tracks  )
    } else {
      recommendations_by_tracks
    }
  } else {
    recommendations_by_artists
  }
}
