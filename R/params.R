#' Recommendation parameters
#'
#' @param playlist_id A Spotify playlist id. This playlist serves as the basis of the recommendation.
#' @param target_nationality Defaults to \code{"sk"}. The nationality id of the recommendation candidate artists, if necessary.
#' @param target_release Defaults to \code{NULL}. The country of origin of the recommendation candidate tracks, if necessary.
#' @param n_rec Number of required recommendations. (Maximum value, may not be fulfilled.)
#' @name recparam
NULL

#' Suitable recommendation candidates.
#'
#' @param target_ids A character vector of the suitable recommendation candidates.
#' @name target_ids
NULL

#' Information about the user playlist
#'
#' An analysis of a user playlist that will serve as the basis of new recommendations.
#'
#' @param user_playlist_info An analysis of a user playlist that will serve as the basis of new recommendations
#' created by \code{get_playlist_information}.
#' @name user_playlist_info
NULL


#' Information about the artists on the user playlist
#'
#' An analysis of a user playlist that will serve as the basis of new recommendations.
#'
#' @param user_playlist_artists A user_playlist_artists data frame from
#' \code{\link{user_playlist_info}}.
#' @name user_playlist_artists
NULL


#' Artist's Unique ID on Spotify
#'
#' An analysis of a user playlist that will serve as the basis of new recommendations.
#'
#' @param spotify_artist_id Artist's Unique ID on Spotify
#' @name spotify_artist_id
#' @rdname spotify_artist_id
NULL
