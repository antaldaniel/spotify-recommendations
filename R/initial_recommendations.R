#' @title Get Initial Recommendations
#'
#' @param playlist_information A list received from get_playlist_information.
#' @param target_ids Spotify artists ids from the targeted repertoire.
#' @importFrom dplyr bind_rows
#' @importFrom spotifyr get_recommendations_all
#' @importFrom purrr possibly
#' @return A tibble of recommendations.
#' @export

initial_recommendations <- function( playlist_information,
                                     target_ids = NA_character_,
                                     limit = 20,
                                     token = NULL ) {

  if (is.null(token)) token <- get_spotify_access_token()

  ### Artist based recommendations ------------------------------

  all_artists <- playlist_information$user_playlist_artists

  if ( length(all_artists$id) > limit ) {
    all_artists <- sample_n ( ungroup(all_artists),
                              size = limit)
  }

  possible_recommendations_by_artists <- purrr::possibly(
    .f = get_recommendations_artists_all,
    otherwise = NULL)

  recommendations_by_artists <- possible_recommendations_by_artists (
    artist_ids = all_artists$id,
    authorization = token )

  if ( is.null(recommendations_by_artists)) {
    warning ("No recommendation was made on the basis of original artists")
  }

  if (length( playlist_information$user_playlist_tracks$track.id) > limit) {
    user_track_ids <- sample_n(
      ungroup( playlist_information$user_playlist_tracks), size = limit )
  } else {
    user_track_ids <-
      ungroup( playlist_information$user_playlist_tracks)

  }

  ### Track based recommendations -----------------------------------
  recommendations_by_tracks <- purrr::possibly(
    .f = spotifyr::get_recommendations_all,
    otherwise = NULL
  )(
    track_ids = unique (user_track_ids$track.id)
  )

  if ( !is.null(recommendations_by_artists)) {
    recommendations_by_artists <- recommendations_by_artists %>%
      mutate ( release_country_code = get_release_country(
        isrc = external_ids.isrc
      ))

    rec_by_artists <- unlist (
      sapply ( recommendations_by_artists$artists, function(x) {
        any ( x$id %in% target_ids) }
      )
    )
    recommendations_by_artists$target_artists <- rec_by_artists
  } else {
    warning ("No recommendation was made on the basis of original artists.")
  }

  if ( ! is.null(recommendations_by_tracks)) {
    recommendations_by_tracks <- recommendations_by_tracks %>%
      mutate ( release_country_code = get_release_country(
        isrc = external_ids.isrc
      ))

    rec_by_tracks <- unlist (
      sapply ( recommendations_by_tracks$artists, function(x) {
        any ( x$id %in% target_ids) }
      )
    )

    recommendations_by_tracks$target_artists <- rec_by_tracks
  } else {
    warning ("No recommendation was made on the basis of original tracks.")
  }

  ## Combination of recommendations --------------------------------
  if ( !is.null(recommendations_by_tracks)) {
    if ( !is.null(recommendations_by_artists)){
       dplyr::bind_rows ( recommendations_by_artists,
                         recommendations_by_tracks  )
    } else {
      recommendations_by_tracks
    }
  } else {
    recommendations_by_artists
  }
}
