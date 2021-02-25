#' @title Get Recommendations For All Artists
#'
#' @param artist_ids Spotify Artist IDs
#' @inheritParams get_local_recommendations
#' @importFrom purrr map_df
#' @importFrom spotifyr get_recommendations
#' @export

get_recommendations_artists_all <- function (
        artist_ids,
        authorization = NULL ) {

  if (is.null(token)) token <- get_spotify_access_token()

  get_recs <- function(i, ids, vec_length ) {
    start <- i
    end <- ifelse(i + 4 > vec_length, vec_length, i + 4)
    seeds <- artist_ids[c(start:end)]
    recs <- spotifyr::get_recommendations(
      limit = (end + 1 - start),
      seed_artists = seeds )
    recs
  }

  artist_length <- length(artist_ids)
  artist_seq <- seq(from = 1, to = artist_length, by = 5)
  all_recs <- purrr::map_df(artist_seq, ~get_recs(.x,
                                                  ids = artist_ids,
                                                  vec_length = artist_length))
  all_recs
}
