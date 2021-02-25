#' Get Nearest Tracks
#'
#' Get recommendations based on genre.
#'
#' @param user_playlist_info A tibble created by \code{\link{get_playlist_information}}.
#' @param new_tracks Tracks to limit.
#' @inheritParams get_local_recommendations
#' @importFrom spotifyr get_playlist_audio_features
#' @importFrom dplyr select filter distinct full_join left_join
#' @importFrom dplyr sample_n bind_rows rename group_by
#' @importFrom tidyselect vars_select_helpers
#' @importFrom stats kmeans
#' @importFrom purrr possibly
#' @return A character vector or artist IDs.
#' @export

get_nearest_tracks <- function( user_playlist_info,
                                new_tracks,
                                n_rec = 5 ) {

  if ( "list" %in% class(user_playlist_info) ) {
    user_playlist_info <- user_playlist_info$user_playlist_tracks
  }

  ## subset the data frames ------------------------------------------
  user_tracks <- user_playlist_info %>%
    select ( any_of(c("track.id","track_id",  "danceability","energy", "key",
                      "instrumentalness",
                      "liveness", "valence", "tempo", "time_signature")) )

  new_tracks <- new_tracks %>%
    select ( any_of(c("track.id","track_id", "danceability","energy","key",
                      "instrumentalness",
                      "liveness", "valence", "tempo", "time_signature")) )

  ## Bind them and scale them ----------------------------------------------

  tracks <- bind_rows(user_tracks, new_tracks) %>%
    select ( vars_select_helpers$where (is.numeric) )

  scaled_new_tracks <- scale(new_tracks %>%
                               select ( vars_select_helpers$where(is.numeric) )
                             )

  ## Cluster user tracks to n clusters, and use the cluster center for n_rec
  ## recommendations and not more.

  user_track_clusters <- stats::kmeans(user_tracks %>%
                                         select ( tidyselect::vars_select_helpers$where (is.numeric) ),  #workaround to reference where
                                       centers=n_rec, iter.max = 100, nstart = 1)

  closest_to_center <- function(centers,df,x) {
    ## find the track that is nearest to each cluster center
    distance_from_center <- rowSums(centers$centers[x,] - df)
    which(distance_from_center  == min( distance_from_center) )
  }

  ## result vector defined -------------------------------------------
  nearest_tracks <- vector (mode = 'integer', length = n_rec)

  ## replace till unique, i.e. if track already recommended, recommend
  ## something else till n unique tracks are recommended  -------------

  for ( i in 1:n_rec ) {

    nearest_tracks[i] <- closest_to_center(centers = user_track_clusters,
                                           df = scaled_new_tracks,
                                           x = i)

    ## avoid repetition -------------------------------------------------
    earlier_identified <- nearest_tracks[-i]
    earlier_identified <- earlier_identified[ earlier_identified >0 ]

    remaining_tracks <- scaled_new_tracks

    while ( any (earlier_identified  %in% nearest_tracks[i]) ) {

      remove_item <- earlier_identified [which(nearest_tracks[-i] %in% nearest_tracks[i])]
      remaining_tracks <- remaining_tracks[-remove_item,]
      if ( nrow(remaining_tracks)==0) break
      nearest_tracks[i] <- closest_to_center(centers = user_track_clusters,
                                             df = remaining_tracks,
                                             x = i)
    }
  }

  ids_to_look_up <- NULL

  if ( "track_id" %in% names(new_tracks) ) {
    ids_to_look_up <- new_tracks$track_id
    } else if ("track_id" %in% names(new_tracks) ) {
      ids_to_look_up <- new_tracks$track_id
    }


 if ( !is.null(ids_to_look_up)) {
   ## add the spotify track features to the selected tracks --------------
   purrr::possibly(.f = spotifyr::get_tracks, NULL)(
     ids_to_look_up[nearest_tracks]
   )
 } else {
   # what shall happen if there is nothing to return
   warning("Returning input tracks.")
   new_tracks }

}
