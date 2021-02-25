library(tidyverse)
library(spotifyrecommendations)

sound_of_dutch_indie_playlist <- spotifyr::get_playlist("2pSeH1YQCSICs2knjs7e5o")
sound_of_dutch_indie_features <- spotifyr::get_playlist_audio_features(
  playlist_uris = "2pSeH1YQCSICs2knjs7e5o",
  authorization = authorization )



usethis::use_data(sound_of_dutch_indie_playlist, overwrite = TRUE)
usethis::use_data(sound_of_dutch_indie_features, overwrite = TRUE)

class ( sound_of_dutch_indie_playlist )

cat ( names ( sound_of_dutch_indie_playlist ))

names ( sound_of_dutch_indie_features )
