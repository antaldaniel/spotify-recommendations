## code to prepare `DATASET` dataset goes here
library(tidyverse)
library(spotifyrecommendations)

spotify_recommendations_dir <- file.path("..", "..", "__SR", "spotify-features", "tables"  )

dir ( spotify_recommendations_dir )



sk_artist_distance <- read.csv(file.path(
  spotify_recommendations_dir, 'SK_Artist_Distances.csv'
))  %>%
  setNames ( ., c("recommendation", "spotify_artist_id", "distance")) %>%
  mutate ( national_identity = "sk")

hu_artist_distance <- read.csv(file.path(
  spotify_recommendations_dir, 'HU_Artist_Distances.csv'
))  %>%
  setNames ( ., c("recommendation", "spotify_artist_id", "distance")) %>%
  mutate ( national_identity = "hu")

nl_artist_distance <- read.csv(file.path(
  spotify_recommendations_dir, 'NL_Artist_Distances.csv'
))  %>%
  setNames ( ., c("recommendation", "spotify_artist_id", "distance")) %>%
  mutate ( national_identity = "nl")

artist_distances <- sk_artist_distance %>%
  dplyr::bind_rows( hu_artist_distance ) %>%
  dplyr::bind_rows( nl_artist_distance )

usethis::use_data(artist_distances, overwrite = TRUE)
