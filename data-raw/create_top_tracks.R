## code to prepare `DATASET` dataset goes here
library(tidyverse)
library(spotifyrecommendations)

spotify_recommendations_dir <- file.path(
  "..", "..", "__SR", "spotify-features", "tables"
)

dir(spotify_recommendations_dir)


sk_top_tracks <- read.csv(file.path(
  spotify_recommendations_dir, 'SK_Artist_Top_Tracks_Table.csv'
))  %>%
  setNames(., c("spotify_artist_id", names(.)[2:ncol(.)]))

nl_top_tracks <- read.csv(file.path(
  spotify_recommendations_dir, 'NL_Artist_Top_Tracks_Table.csv'
))  %>%
  setNames(., c("spotify_artist_id", names(.)[2:ncol(.)]))

hu_top_tracks <- read.csv(file.path(
  spotify_recommendations_dir, 'HU_Artist_Top_Tracks_Table.csv'
))  %>%
  setNames(., c("spotify_artist_id", names(.)[2:ncol(.)]))

top_tracks <- sk_top_tracks  %>%
  mutate ( national_identity  = "sk") %>%
  bind_rows(nl_top_tracks %>% mutate ( national_identity = "nl") ) %>%
  bind_rows(hu_top_tracks %>% mutate ( national_identity = "hu"))

spotify_top_tracks <- top_tracks %>%
  rename ( spotify_artist_name = artist_name,
           spotify_popularity = popularity,
           spotify_genres = genres ) %>%
  select ( -all_of(c('genre_1', "genre_2", "genre_3", "followers",
                     "followers" ))) %>%
  mutate ( date = Sys.Date())


listen_local_artists <- sk_top_tracks %>%
  select ( all_of (c(
    "spotify_artist_id", "name", "all_english_title",
    "all_slovak_title", "any_slovak_title",
    "is_considered_slovak", "considered_czech", "known_slovak_city",
    "genre_1", "genre_2", "genre_3"
  ))) %>%
  mutate (
    language = case_when (
      all_english_title == 1 ~ "en",
      all_slovak_title == 1 ~ "sk",
      TRUE ~ NA_character_),
    national_identity = case_when (
      considered_czech == 1 ~ "cz",
      is_considered_slovak == 1 ~ "sk",
      )
    ) %>%
  rename ( city = known_slovak_city,
           artist_name = name ) %>%
  distinct ( spotify_artist_id, .keep_all = TRUE ) %>%
  select ( all_of (c("spotify_artist_id", "artist_name",
                     "national_identity","language",
                     "genre_1", "genre_2", "genre_3")))

usethis::use_data(listen_local_artists, overwrite = TRUE)

conn <- dbConnect(RSQLite::SQLite(), "not_included/listen-local.db")
dbWriteTable(conn, "spotify_top_tracks", spotify_top_tracks )
dbListTables(conn)
dbDisconnect(conn)


