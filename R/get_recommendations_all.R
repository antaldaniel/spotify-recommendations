#' Get recommendations for a submitted vector of track IDs, with no limit on the number of seed tracks
#'
#' @description
#' This is a wrapper for the get_recommendations() function,
#' which provides a workaround for the limit of 5 seed tracks per
#' recommendation call. The function splits a supplied vector of
#' track IDs into subsets of length 5, then applies a
#' get_recommendations() call, 5 tracks at a time.
#' This should generate a data frame of recommended tracks, with length equal to the supplied vector of track ids.
#'
#'
#' @param track_ids
#' A vector containing the IDs of the tracks you'd like recommendations for
#' @param valence
#' The target valence for the recommendations
#'
#' @return
#' Returns a data frame containing recommendations from the Spotify API
#' @export
#'
#' @examples
#' @importFrom spotifyr get_recommendations_all
#' \dontrun{
#' get_recommendations_all(c("5VIpLopHgolKcSSj7JPCMA", "3QRGYDFFUVb4qneE4DX1gR"))
#' }

get_recommendations_all <- spotifyr::get_recommendations_all
