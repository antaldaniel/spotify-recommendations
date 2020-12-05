#' @title Get Recommendations Based On Genre
#'
#' Get recommendations based on genre.
#'
#' @param artists_by_genre A tibble created by \code{\link{get_artist_genre}}.
#' @param target_nationality One of \code{'cz'} or \code{'sk'}.
#' @importFrom spotifyr get_playlist_audio_features
#' @importFrom dplyr select filter distinct full_join left_join
#' @importFrom dplyr sample_n bind_rows rename group_by
#' @return A character vector or artist IDs.
#' @export

get_artist_recommendations_genre <- function(
      artists_by_genre,
      target_nationality = "sk" ) {

  data ( "local_genre_table", envir=environment())
  data ( "listen_local_artists", envir=environment())

  ll <- listen_local_artists %>%
    dplyr::filter ( national_identity == target_nationality)

  l_genre <- local_genre_table %>%
    dplyr::filter ( national_identity == target_nationality)

  tmp <- l_genre %>%
    full_join ( artists_by_genre  %>%
                  distinct ( genre, spotify_artist_id ),
                by = "genre") %>%
    dplyr::filter ( complete.cases(.)) %>%
    dplyr::rename ( base_spotify_artist_id = spotify_artist_id ) %>%
    full_join ( ll, by = "national_identity" ) %>%
    distinct ( base_spotify_artist_id, spotify_artist_id, distance ) %>%
    ungroup() %>%
    group_by( base_spotify_artist_id, distance ) %>%
    sample_n( size = 1 )

  tmp %>% ungroup() %>%
    filter ( distance == min (distance)) %>%
    bind_rows ( tmp %>% ungroup %>%
                  filter (distance > min(distance, na.rm=TRUE))) %>%
    ungroup () %>%
    slice_head ( n = nrow (artists_by_genre) ) %>%
    select ( all_of("spotify_artist_id")) %>%
    unlist() %>% as.character()
}
