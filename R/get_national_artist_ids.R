#' Get Artists IDs For Target Nationality.
#'
#' Get unique tracks and unique artists from a playlist in a list.
#'
#' @param target_identity One of \code{'sk'} or \code{'nl'} or \code{'hu'}.
#' @importFrom spotifyr get_playlist_audio_features
#' @importFrom dplyr select filter distinct
#' @importFrom tidyselect all_of
#' @importFrom utils data
#' @return A character vector or artist IDs.
#' @export

get_national_artist_ids <- function( target_identity = 'sk' ) {

  if (! exists("listen_local_artists") ) {
   data(listen_local_artists, envir =  environment())
  }

  target_artists <- listen_local_artists %>%
    dplyr::filter ( .data$national_identity == target_identity )   %>%
    dplyr::select ( all_of("spotify_artist_id") ) %>%
    dplyr::distinct () %>%
    unlist() %>% as.character()
}
