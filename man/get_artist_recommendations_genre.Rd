% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_artist_recommendations_genre.R
\name{get_artist_recommendations_genre}
\alias{get_artist_recommendations_genre}
\title{Get Recommendations Based On Genre

Get recommendations based on genre.}
\usage{
get_artist_recommendations_genre(artists_by_genre, target_nationality = "sk")
}
\arguments{
\item{artists_by_genre}{A tibble created by \code{\link{get_artist_genre}}.}

\item{target_nationality}{Defaults to \code{"sk"}. The nationality id of the recommendation candidate artists, if necessary.}
}
\value{
A character vector or artist IDs.
}
\description{
Get Recommendations Based On Genre

Get recommendations based on genre.
}
