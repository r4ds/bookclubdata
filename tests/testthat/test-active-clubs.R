test_that("Can update active clubs", {
  raw_sheet_tibble <- tibble::tibble(
    Club = c("A01", "B01"),
    "Day (UTC)" = c("Monday", "Tuesday"),
    "Hour (UTC)" = c(1, 2)
  )
  datetimes <- c(
    lubridate::ymd_hms("2023-11-06 01:00:00 UTC"),
    lubridate::ymd_hms("2023-11-07 02:00:00 UTC")
  )
  clean_tibble <- tibble::tibble(
    club = c("a01", "b01"),
    day_utc = c("Monday", "Tuesday"),
    hour_utc = c(1L, 2L),
    datetime_utc = datetimes
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
    .rds_update = function(...) invisible(),
    make_datetimes_utc = function(...) {
      message("Making datetimes")
      return(datetimes)
    }
  )
  expect_message(expect_message(expect_message(
    {
      test_result <- active_clubs_times()
    },
    "Reading from x, sheet raw_clubs"),
    "Downloading googledrive file x"),
    "Making datetimes"
  )
  expect_identical(test_result, clean_tibble)
  memoise::forget(.cached_sheet_impl)
  memoise::forget(.rds_read_impl)
  memoise::forget(active_clubs_times)
})

test_that("Can fetch active clubs", {
  raw_sheet_tibble <- tibble::tibble(
    Club = c("A01", "B01"),
    "Day (UTC)" = c("Monday", "Tuesday"),
    "Hour (UTC)" = c(1, 2)
  )
  datetimes <- c(
    lubridate::ymd_hms("2023-11-06 01:00:00 UTC"),
    lubridate::ymd_hms("2023-11-07 02:00:00 UTC")
  )
  clean_tibble <- tibble::tibble(
    club = c("a01", "b01"),
    day_utc = c("Monday", "Tuesday"),
    hour_utc = c(1L, 2L),
    datetime_utc = datetimes
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
    },
    make_datetimes_utc = function(...) {
      message("Making datetimes")
      return(datetimes)
    }
  )
  expect_message(expect_message(
    {
      test_result <- active_clubs_times()
    },
    "Downloading googledrive file .+"),
    "Making datetimes"
  )
  expect_identical(test_result, clean_tibble)
  memoise::forget(.cached_sheet_impl)
  memoise::forget(.rds_read_impl)
  memoise::forget(active_clubs_times)
})

test_that("Re-fetch memoisation works", {
  local_mocked_bindings(
    .cached_sheet_fetch = function(...) {
      message("Checking cache")
    },
    make_datetimes_utc = function(...) {
      message("Making datetimes")
    }
  )
  expect_message(expect_message(
    {
      test_result <- active_clubs_times()
    },
    "Checking cache"
  ), "Making datetimes")
  expect_no_message({
    test_result <- active_clubs_times()
  })
  expect_message(expect_message(
    {
      test_result <- active_clubs_times(effective_date = "anything new")
    },
    "Checking cache"
  ), "Making datetimes")
  memoise::forget(.cached_sheet_impl)
  memoise::forget(.rds_read_impl)
  memoise::forget(active_clubs_times)
})
