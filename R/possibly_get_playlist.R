
#' @importFrom spotifyr get_playlist get_spotify_access_token
#' @importFrom purrr possibly
#' @keywords internal

possibly_get_playlist <- function(playlist_id,
                                  fields = NULL,
                                  market = NULL,
                                  authorization = get_spotify_access_token()
                                  ) {

  if ( playlist_id == "sound_of_dutch_indie_playlist") {
    data("sound_of_dutch_indie_playlist", envir=environment())
    return(sound_of_dutch_indie_playlist)
  }

  pgp <- purrr::possibly(
    .f = spotifyr::get_playlist, NULL)


  pgp ( playlist_id, fields, market, authorization)
}

