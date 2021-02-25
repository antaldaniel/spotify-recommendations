#' Get Recommendations for an Artist
#'
#' @inheritParams spotify_artist_id
#' @inheritParams get_local_recommendations
#' @importFrom dplyr bind_rows mutate filter select distinct
#' @importFrom tidyselect all_of
#' @importFrom purrr possibly
#' @importFrom spotifyr get_spotify_access_token get_artist_top_tracks
#' @importFrom stats complete.cases
#' @importFrom utils data
#' @return A tibble of recommendations.
#' @export

get_track_recommendations_artist <- function (
     spotify_artist_id,
     user_playlist_info,
     target_nationality = "sk",
     target_release = NULL,
     n_rec = 5,
     authorization = NULL) {

  if (is.null(authorization)) authorization <- get_spotify_access_token()

  data("listen_local_artists", envir=environment())

  if ( !is.null(target_nationality) ) {
    target_artists <- get_national_artist_ids ( .data$target_nationality )
  }

  recs <- get_recommendations_artists_all(artist_ids = spotify_artist_id )

  get_top_tracks <- function (artist_id) {

    top_tracks <- purrr::possibly(
      .f = get_artist_top_tracks, NULL)(artist_id,
                                        authorization = authorization)

    message ( artist_id )

    fn_detect_artists <- function(x) {
      ifelse ( any (x %in% target_artists), TRUE, FALSE)
    }

  if (is.null(top_tracks))      return(top_tracks)
  if (length ( top_tracks )==0) return(top_tracks)

  if (!is.null(target_release)) {
      top_tracks <- top_tracks %>% mutate (
        release_country = get_release_country(.data$external_ids.isrc)) %>%
        filter ( tolower(.data$release_country) == tolower(target_release) ) %>%
        select ( -all_of("release_country") )
    }

  if (!is.null(target_nationality)) {
      top_tracks <- top_tracks[sapply ( top_tracks$artists, function(x) fn_detect_artists(x) ),]
    }

  if ( !is.null(top_tracks)) {

    if (nrow(top_tracks)==0) return(NULL)

      top_tracks %>%
        mutate ( spotify_artist_id = artist_id ) %>%
        #dplyr::filter ( is_playable ) %>%
        dplyr::select ( all_of(c("spotify_artist_id", "id", "name", "popularity",
                                 "uri", "album.id", "album.name", "album.album_type",
                                 "external_ids.isrc")))
    }

  }



  tmp <- lapply ( sample ( spotify_artist_id, n_rec ), get_top_tracks)

  recommendations <- do.call ( rbind, tmp ) %>%
    distinct ( .data$external_ids.isrc, .keep_all = TRUE ) %>%
    mutate ( release_country_code = get_release_country(.data$external_ids.isrc),
             )

  recommendations %>%
      mutate (
        target_artists = ifelse (is.null(.data$target_nationality),
                                     TRUE,
                                     spotify_artist_id %in% get_national_artist_ids(.data$target_nationlity))
      )

}
