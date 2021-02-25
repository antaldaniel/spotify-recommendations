library(tidyverse)
library(spotifyrecommendations)
library(DBI)

disc_con <- dbConnect(RSQLite::SQLite(), "not_included/listen-local.db")
dbListTables(disc_con)
local_genre_table <- dbReadTable(disc_con, "local_genre_table") %>%
  select (-all_of("national_identity"))
usethis::use_data(local_genre_table, overwrite = TRUE)
dbDisconnect(disc_con)
