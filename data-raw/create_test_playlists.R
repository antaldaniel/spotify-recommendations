library(tidyverse)
library(spotifyrecommendations)

sound_of_dutch_indie_playlist <- spotifyr::get_playlist("2pSeH1YQCSICs2knjs7e5o")

usethis::use_data(sound_of_dutch_indie_playlist, overwrite = TRUE)

class ( sound_of_dutch_indie_playlist )

cat ( names ( sound_of_dutch_indie_playlist ))
