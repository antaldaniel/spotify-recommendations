library(tidyverse)
library(spotifyrecommendations)
library(DBI)

disc_con <- dbConnect(RSQLite::SQLite(), "not_included/listen-local.db")
dbListTables(disc_con)
listen_local_artists <- dbReadTable(disc_con, "spotify_top_tracks") %>%
  select ( all_of (c(
    "spotify_artist_id", "spotify_artist_name", "national_identity"
  ))) %>%
  dplyr::distinct_all()



usethis::use_data(listen_local_artists, overwrite = TRUE)

names ( listen_local_artists)

genre_long_form <- function(artist_row) {
  y <- artist_row %>% select ( -all_of("spotify_genres") )
  x <- gsub("â€™", "'", artist_row$spotify_genres)
  x <- gsub("(?<!' )(?i)\\]", "']", x, perl = TRUE) #negative lookbehind for incomplete
  x <- gsub("\\[", "\\(", x)
  x <- gsub("\\]", "\\)", x)
  x <- gsub("''", "'", x)

  if ( grepl("\\,", x) ) {
    x <- paste0("c", x)
    x <- eval(parse(text=x))
  } else {
    x <- gsub("\\'|\\(|\\)", "", x)
  }
  n_row <- length(x)


  if ( n_row < 1 ) {
    ## return artist even if there is no genre recorded
    x <- NA_character_
    n_row <- 1 }

  return_df <- data.frame (
    spotify_artist_id   = rep (y$spotify_artist_id, n_row ),
    spotify_artist_name = rep (y$spotify_artist_name, n_row ),
    national_identity  = rep (y$national_identity, n_row ),
    spotify_genres = x)

  return_df

}

listen_local_artists_genre <- dbReadTable(disc_con, "spotify_top_tracks") %>%
  select ( all_of (c(
    "spotify_artist_id", "spotify_artist_name", "national_identity",
    "spotify_genres"
  ))) %>%
  dplyr::distinct_all()

n_elements <- nrow(listen_local_artists_genre)
for ( i in 1:n_elements) {
  if (i > 1) {
    artist_genre_table <- rbind(
      artist_genre_table,
      genre_long_form(artist_row = listen_local_artists_genre[i,])
    )
  } else {
    artist_genre_table  <- genre_long_form(artist_row = listen_local_artists_genre[i,])
  }

  if ( i %% 100 == 0) message ( i, " / ", n_elements )
}

usethis::use_data(artist_genre_table, overwrite = TRUE)
dbDisconnect(disc_con)
