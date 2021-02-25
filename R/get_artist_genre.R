#' @title Get Artist Genre
#'
#' Get the genres of the artists, if available.
#'
#' @param user_playlist_artists A user_playlist_artists data frame from
#' \code{\link{user_playlist_info}}.
#' @param max_n Maximum number of artists to work with, defaults to \code{20}.
#' @importFrom spotifyr get_playlist_audio_features
#' @importFrom dplyr select filter distinct full_join left_join
#' @importFrom dplyr slice_head group_by ungroup arrange
#' @importFrom tibble tibble
#' @importFrom spotifyr get_artists
#' @return A tibble of artist IDs and genres in long form.
#' @export

get_artist_genre <- function( user_playlist_artists,
                              max_n = 20 ) {

  . <- NULL

  artist_ids <- user_playlist_artists %>%
    arrange ( -.data$n ) %>%
    ungroup() %>%
    dplyr::slice_head (
      #work with up to 20 artists max
      n=max_n ) %>%
    select ( all_of ("id") ) %>%
    unlist() %>%
    as.character()

  artist_info <- spotifyr::get_artists(ids = artist_ids )

  get_genre <- function (x) {

    if ( is.null(x$genres) ) {
      tibble ( genre             = NA_character_,
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

  # organize list into a data.frame
  do.call(rbind, apply (artist_info, 1, get_genre ) )

}
