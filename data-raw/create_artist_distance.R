library(tidyverse)
library(spotifyrecommendations)

disc_con <- dbConnect(RSQLite::SQLite(), "not_included/listen-local.db")
dbListTables(disc_con)
artist_distances  <- dbReadTable(disc_con, "artist_distances")

usethis::use_data(artist_distances, overwrite = TRUE)
dbDisconnect(disc_con)
