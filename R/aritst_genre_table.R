#' Listen Local Artists' Genres
#'
#' A dataset containing national identity and genres of the listen local artists.
#'
#' @format A data frame with 1247 rows and 4 variables:
#' \describe{
#'   \item{spotify_artist_id}{Spotify artist unique identifier.}
#'   \item{spotify_artist_name}{The name of the artist on Spotify.}
#'   \item{national_identity}{Assumed national identity of the artist(s).}
#'   \item{spotify_genres}{A vector of genres as identified by Spotify.}
#' }
#' @source \url{http://listenlocal.info/}
"artist_genre_table"
