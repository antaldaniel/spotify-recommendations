#' Upload Playlist
#'
#' Upload the recommended playlist to the user's Spotify profile.
#'
#' @param user_id A single Spotify playlist id
#' @param playlist_name Name for a new playlist
#' @param playlist_description New description for the playlist
#' @param track_uris The track uris of the new playlist.
#' @param authorization Defaults to \code{NULL} when
#' \code{\link[spotifyr]{get_spotify_access_token}} is invoked.
#' @importFrom spotifyr get_user_playlists add_tracks_to_playlist
#' @importFrom dplyr filter
#' @return Does not return anything.
#' @export

upload_playlist <- function (
  user_id = NULL,
  playlist_name = 'test_listen_local_playlist',
  playlist_description = "A test playlist of the Listen Local App",
  track_uris = NULL,
  authorization = NULL ) {

  if (is.null(authorization)) {
    authorization <- spotifyr::get_spotify_access_token()
    }

  new_playlist <- spotifyr::create_playlist(user_id,
                            name = playlist_name,
                            description = playlist_description )

  added <- spotifyr::add_tracks_to_playlist(playlist_id = new_playlist$id,
                                   uris = track_uris )
  added
}
