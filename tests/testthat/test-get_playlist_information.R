library(testthat)
sound_of_dutch_indie_playlist_id <- "2pSeH1YQCSICs2knjs7e5o"
slovakia_indie_playlist_id       <- "1E8vQ9Bt4bRKWm0NTzSMt2"
slovakia_top_50_playlist_id      <- "6WrpeNyqtoq6PEDXyuamGt"
sound_of_dutch_indie_playlist_id <- "2pSeH1YQCSICs2knjs7e5o"

possibly_get_playlist('sound_of_dutch_indie_playlist')

tested_playlist_info <- get_playlist_information(playlist_id = 'sound_of_dutch_indie_playlist')

test_that("Correct format is returned", {
  expect_equal(class ( tested_playlist_info ), "list")
  expect_equal(length( tested_playlist_info ), 3)
})





