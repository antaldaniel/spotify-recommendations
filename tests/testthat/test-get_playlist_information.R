library(testthat)
sound_of_dutch_indie_playlist_id <- "2pSeH1YQCSICs2knjs7e5o"
slovakia_indie_playlist_id       <- "1E8vQ9Bt4bRKWm0NTzSMt2"
slovakia_top_50_playlist_id      <- "6WrpeNyqtoq6PEDXyuamGt"
sound_of_dutch_indie_playlist_id <- "2pSeH1YQCSICs2knjs7e5o"

possibly_get_playlist('sound_of_dutch_indie_playlist')

get_playlist_information(playlist_id = 'sound_of_dutch_indie_playlist')

test_that("Correct format is returned", {
  expect_equal(2 * 2, 4)
})


sound_of_dutch_indie_audio_features <- spotifyr::get_playlist_audio_features(
  playlist_uris = sound_of_dutch_indie_playlist_id,
  authorization = authorization )


playlist_id <- "2pSeH1YQCSICs2knjs7e5o"
