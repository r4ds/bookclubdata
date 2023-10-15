# This is *only* testing things specifically in active-clubs.R.

test_that("Can update active clubs", {
  raw_sheet_tibble <- tibble::tibble(
    Club = c("A01", "B01"),
    "Day (UTC)" = c("Monday", "Tuesday"),
    "Hour (UTC)" = c(1, 2)
  )
  clean_tibble <- tibble::tibble(
    club = c("a01", "b01"),
    day_utc = c("Monday", "Tuesday"),
    hour_utc = c(1L, 2L)
  )
  local_mocked_bindings(
    .rds_timestamp = function(...) 1,
    .googledrive_download = function(file, path) {
      message("Downloading googledrive file x")
      saveRDS(clean_tibble, path)
    },
    .googledrive_timestamp = function(...) 2,
    .googlesheet_read = function(...) {
      message("Reading from x, sheet raw_clubs")
      raw_sheet_tibble
    },
    .rds_update = function(...) invisible()
  )
  expect_message(
    expect_message(
      {
        test_result <- active_clubs_times()
      },
      "Reading from x, sheet raw_clubs"
    ),
    "Downloading googledrive file x"
  )
  expect_identical(test_result, clean_tibble)
  memoise::forget(.active_clubs_times_impl)
  memoise::forget(.active_clubs_rds_read)
})

test_that("Can fetch active clubs", {
  raw_sheet_tibble <- tibble::tibble(
    Club = c("A01", "B01"),
    "Day (UTC)" = c("Monday", "Tuesday"),
    "Hour (UTC)" = c(1, 2)
  )
  clean_tibble <- tibble::tibble(
    club = c("a01", "b01"),
    day_utc = c("Monday", "Tuesday"),
    hour_utc = c(1L, 2L)
  )
  local_mocked_bindings(
    .rds_timestamp = function(...) 2,
    .googledrive_download = function(file, path) {
      message("Downloading googledrive file x")
      saveRDS(clean_tibble, path)
    },
    .googledrive_timestamp = function(...) 1,
    .googlesheet_read = function(...) {
      message("Reading from x, sheet Clubs")
      raw_sheet_tibble
    }
  )
  expect_message(
    {test_result <- active_clubs_times()},
    "Downloading googledrive file .+"
  )
  expect_identical(test_result, clean_tibble)
  memoise::forget(.active_clubs_times_impl)
  memoise::forget(.active_clubs_rds_read)
})

test_that("Re-fetch memoisation works", {
  local_mocked_bindings(
    .rds_timestamp = function(...) 2,
    .active_clubs_rds_read = function(...) {
      message("Reading rds")
    },
    .googledrive_timestamp = function(...) 1
  )
  expect_message(
    {test_result <- active_clubs_times()},
    "Reading rds"
  )
  expect_no_message({test_result <- active_clubs_times()})
  expect_message(
    {test_result <- active_clubs_times(refresh = TRUE)},
    "Reading rds"
  )
  memoise::forget(.active_clubs_times_impl)
  memoise::forget(.active_clubs_rds_read)
})
