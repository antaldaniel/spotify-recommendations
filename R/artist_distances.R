#' Artist Distances
#'
#' A dataset containing related artists.
#'
#' @format A data frame with 118981 rows and 4 variables:
#' \describe{
#'   \item{spotify_artist_id}{Spotify artist unique identifier.}
#'   \item{recommendation}{Recommendation based on spotify_artist_id}
#'   \item{distance}{Distance between base artist and recommended artist.}
#'   \item{national_identity}{Assumed national identity of the artist(s).}
#' }
#' @source \url{http://listenlocal.info/}
"artist_distances"
