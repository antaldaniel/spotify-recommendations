#' @title Get Recommendations Based On Genre
#'
#' Get recommendations based on genre.
#'
#' @param artists_by_genre A tibble created by \code{\link{get_artist_genre}}.
#' @inheritParams get_local_recommendations
#' @inheritParams spotify_artist_id
#' @inheritParams user_playlist_artists
#' @importFrom spotifyr get_playlist_audio_features
#' @importFrom dplyr select filter distinct full_join left_join rename distinct_all
#' @importFrom dplyr sample_n bind_rows rename group_by
#' @import rlang
#' @return A character vector or artist IDs.
#' @export

get_artist_recommendations_genre <- function(
      artists_by_genre,
      target_nationality = "sk" ) {

  if ( ! exists("local_genre_table")) {
    data ( "local_genre_table", envir=environment())
  }

  if ( ! exists("listen_local_artists") ) {
    data ( "listen_local_artists", envir=environment())
  }

  if ( ! exists("artist_genre_table") ) {
    data ( "artist_genre_table", envir=environment())
  }

  if ( !is.null(target_nationality) ) {
    ll <- listen_local_artists %>%
      dplyr::filter ( .data$national_identity == target_nationality )
  } else {
    ll <- listen_local_artists
  }

  base_genre_distances <- local_genre_table  %>%
    full_join ( artists_by_genre  %>%
                  distinct ( .data$genre, .data$spotify_artist_id ) %>%
                  rename ( base_spotify_artist_id = .data$spotify_artist_id ),
                by = 'genre'
                )

  base_genre_distances <- base_genre_distances[complete.cases(base_genre_distances),] %>%
    distinct_all()

  ll_artist_genre_distances <- artist_genre_table %>%
    rename ( genre_rec = .data$spotify_genres )

  tmp <- base_genre_distances  %>%
    full_join ( ll_artist_genre_distances, by = 'genre_rec' ) %>%
    distinct ( .data$base_spotify_artist_id, .data$spotify_artist_id, .data$distance )

  tmp <- tmp[complete.cases(tmp),]

  return_by_genre <- NULL

  if ( nrow(tmp)<1 ) return(return_by_genre)

  seed_n <- min ( c(nrow(tmp), 5) )

  . <- NULL

  min_distance_tmp <- tmp[which ( min(tmp$distance) == tmp$distance),]

  if ( seed_n > nrow(min_distance_tmp) ) {
    return_by_genre <- tmp %>%
      arrange ( .data$distance ) %>%
      .[(1:seed_n), ] %>%
      select ( .data$spotify_artist_id ) %>%
      unlist %>% as.character()
  } else if  ( seed_n <= nrow(min_distance_tmp) ) {
    return_by_genre <- min_distance_tmp %>%
      slice_head(n = seed_n ) %>%
      select ( .data$spotify_artist_id ) %>%
      unlist %>% as.character()
  }

  return_by_genre
}
