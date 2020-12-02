#' @title Upload Playlist
#'
#' Upload the recommended playlist to the user's Spotify profile.
#'
#' @param user_id A single Spotify playlist id
#' @param playlist_name Name for a new playlist
#' @param playlist_description New description for the playlist
#' @param track_uris The track uris of the new playlist.
#' @importFrom spotifyr get_user_playlists add_tracks_to_playlist
#' @importFrom dplyr filter
#' @return Does not return anything.
#' @export

upload_playlist <- function (
  user_id = NULL,
  playlist_name = 'test_listen_local_playlist',
  playlist_description = "A test playlist of the Listen Local App",
  track_uris = NULL) {

  spotifyr::create_playlist(user_id,
                            name = playlist_name,
                            description = "Listen Local Test")

  user_playlists <- spotifyr::get_user_playlists(user_id = 'rx4xjay1368opqg2i7nabuo5c')
  target_playlist <- user_playlists %>%
    filter ( name == new_name,
             description == new_description)

  spotifyr::add_tracks_to_playlist(playlist_id = target_playlist$id,
                                   uris = track_uris )
}
