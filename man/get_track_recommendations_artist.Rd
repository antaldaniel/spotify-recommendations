% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_track_recommendations_artist.R
\name{get_track_recommendations_artist}
\alias{get_track_recommendations_artist}
\title{Get Recommendations for an Artist}
\usage{
get_track_recommendations_artist(
  spotify_artist_id,
  user_playlist_info,
  target_nationality = "sk",
  target_release = NULL,
  n_rec = 5,
  authorization = NULL
)
}
\arguments{
\item{spotify_artist_id}{Artist's Unique ID on Spotify}

\item{target_nationality}{Defaults to \code{"sk"}. The nationality id of the recommendation candidate artists, if necessary.}

\item{target_release}{Defaults to \code{NULL}. The country of origin of the recommendation candidate tracks, if necessary.}

\item{n_rec}{Number of required recommendations. (Maximum value, may not be fulfilled.)}

\item{authorization}{Defaults to \code{NULL} when
\code{\link[spotifyr]{get_spotify_access_token}} is invoked.}
}
\value{
A tibble of recommendations.
}
\description{
Get Recommendations for an Artist
}
