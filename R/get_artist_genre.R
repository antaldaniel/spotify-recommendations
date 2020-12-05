#' @title Get Artist Genre
#'
#' Get the genres of the artist, if available.
#'
#' @param user_playlist_artists A user_playlist_artists data frame from
#' \code{\link{user_playlist_info}}
#' @importFrom spotifyr get_playlist_audio_features
#' @importFrom dplyr select filter distinct full_join left_join
#' @importFrom dplyr slice_head group_by ungroup arrange
#' @importFrom tibble tibble
#' @importFrom spotifyr get_artists
#' @return A tibble of artist IDs and genres in long form.
#' @export

get_artist_genre <- function( user_playlist_artists ) {

  . <- n <-  NULL

  artist_info <- user_playlist_artists %>%
    arrange ( -n ) %>%
    ungroup() %>%
    dplyr::slice_head ( n=20 ) %>%
    select ( all_of ("id") ) %>%
    unlist() %>% as.character() %>%
    get_artists(ids = .)

  get_genre <- function (x) {

    if ( is.null(x$genres) ) {
      tibble ( genre = NA_character_,
               spotify_artist_id = x$id )
    } else{
      tmp <- tibble ( genre =  unlist(x$genres) )

      if ( nrow(tmp)>0 ) {
        tmp   %>%
          mutate ( n = paste0("genre_", 1:nrow(.)),
                   spotify_artist_id = x$id )
      } else { tmp }
    }
  }
  do.call(rbind, apply (artist_info, 1, get_genre ) )
}
