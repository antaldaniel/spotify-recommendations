#' @title Get recommendations for an artist
#'
#' @param spotify_artist_id A vector of artist IDs
#' @param target_nationality Defaults to \code{"sk"}
#' @param target_release If recommendation should be limited to target,
#' defaults to \code{NULL}
#' @param n Number of recommended tracks needed.
#' @param authorization Defaults to \code{NULL} when
#' \code{get_spotify_access_token()} is invoked.
#' @importFrom dplyr bind_rows mutate filter select distinct
#' @importFrom tidyselect all_of
#' @importFrom purrr possibly
#' @importFrom spotifyr get_spotify_access_token
#' @importFrom stats data complete.cases
#' @return A tibble of recommendations.
#' @export

get_track_recommendations_artist <- function (
     spotify_artist_id,
     target_nationality = "sk",
     target_release = NULL,
     n = 5,
     authorization = NULL) {

  if (is.null(authorization)) authorization <- get_spotify_access_token()
  . <- target_nationality <- external_ids.isrc <- release_country <- NULL

  data("listen_local_artists", envir=environment())

  if ( !is.null(target_nationality) ) {
    target_artists <- get_national_artist_ids ( target_nationality )
    }

  get_top_tracks <- function (artist_id) {

    top_tracks <- purrr::possibly(
      .f = get_artist_top_tracks, NULL)(artist_id,
                                        authorization = authorization)

    message ( artist_id )
    fn_detect_artists <- function(x) {
      ifelse ( any (x %in% target_artists), TRUE, FALSE)
    }

  if (is.null(top_tracks)) return(top_tracks)
  if (length ( top_tracks )==0)return(top_tracks)

  if (!is.null(target_release)) {
      top_tracks <- top_tracks %>% mutate (
        release_country = get_release_country(external_ids.isrc)) %>%
        filter ( tolower(release_country) == tolower(target_release)) %>%
        select (-all_of("release_country"))
    }

    if (!is.null(target_nationality)) {
      top_tracks <- top_tracks[sapply ( top_tracks$artists, function(x) fn_detect_artists(x) ),]
    }

    if ( !is.null(top_tracks)) {

      top_tracks %>%
        mutate ( spotify_artist_id = artist_id ) %>%
        #dplyr::filter ( is_playable ) %>%
        dplyr::select ( all_of(c("spotify_artist_id", "id", "name", "popularity",
                                 "uri", "album.id", "album.name", "album.album_type",
                                 "external_ids.isrc")))
    }

  }

  #sample_ids <- sample ( spotify_artist_id, n )
  #for ( i in 1:6 ) { get_top_tracks(artist_id = sample_ids[i] ) }
  #for debugging. Some artists have zero top tracks
  tmp <- lapply ( sample ( spotify_artist_id, n ), get_top_tracks)

  recommendations <- do.call ( rbind, tmp ) %>%
    distinct ( external_ids.isrc, .keep_all = TRUE ) %>%
    mutate ( release_country_code = get_release_country(external_ids.isrc),
             )

  recommendations %>%
      mutate (
        target_artists = ifelse (is.null(target_nationality),
                                     TRUE,
                                     spotify_artist_id %in% get_national_artist_ids(target_nationlity))
      )

}
