#' Get Slovak Artists IDs
#'
#' Recommend local artists from user_playlist_artist data frame.
#'
#' @param user_playlist_artists A character vector of Spotify artist identifiers.
#' @inheritParams get_local_recommendations
#' @rdname target_ids
#' @importFrom spotifyr get_playlist_audio_features
#' @importFrom dplyr select filter distinct anti_join sample_n arrange
#' @importFrom dplyr arrange rename left_join group_by slice_head slice_sample slice_max
#' @importFrom dplyr full_join
#' @importFrom tidyselect all_of
#' @importFrom spotifyr get_spotify_access_token
#' @import rlang
#' @return Returns a character vector of n or less artist ids.
#' @export

get_local_artist_recommendations <- function(
                   user_playlist_artists,
                   target_ids,
                   n_rec = 5,
                   authorization = NULL) {

  if (is.null(authorization)) token <- get_spotify_access_token()

  user_playlist_artists

  if ( ! exists('artist_distances') ) {
    # It should exist in the global environment of the Shiny App
    # see Objects visible across all sessions
    # https://shiny.rstudio.com/articles/scoping.html
    data ( "artist_distances", envir=environment() )
  }

  artist_distance_table <- artist_distances %>%
    filter ( .data$spotify_artist_id %in% user_playlist_artists ) %>%
    filter ( .data$recommendation %in% target_ids) %>%
    filter ( !is.na(.data$recommendation) ) %>%
    arrange ( .data$distance )

  return_seed_size <- ifelse ( test = nrow(artist_distance_table)>n_rec,
                               yes  = n_rec,
                               no   = nrow(artist_distance_table) )

  ## Create a minimum distance seed --------------------------------------------------

  if (nrow(artist_distance_table)==0) {
    warning ( "No recommendation was made by get_local_artist_recommendations()" )
    return(NULL)
  }

  min_rec <- artist_distance_table  %>%
    filter ( distance == min(.data$distance,na.rm=TRUE) ) %>%
    distinct ( .data$recommendation ) %>%
    slice_sample (n = min(c(return_seed_size, nrow(.data))))

  # all( min_rec$recommendation %in% target_artist_ids )

  if ( nrow(min_rec)< n_rec/3 ) {
    ## If there are too few artists, add new ones --------------------

    min_rec <- artist_distance_table  %>%
      distinct (.data$recommendation, .data$distance) %>%
      slice_max ( order_by = -.data$distance,
                  n = min(c(return_seed_size, nrow(min_rec))) )

  }

  return(min_rec$recommendation)
}

