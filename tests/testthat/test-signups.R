test_that("Can update signups", {
  raw_sheet_tibble <- tibble::tibble(
    user_name = c("x", "x", "y"),
    user_id = c("x1", "x1", "y1"),
    book_name = c("a", "a", "b"),
    timezone = c("America/New_York", "America/New_York", "Europe/Berlin"),
    submission_timestamp = c(
      "2023-08-01 12:00:00",
      "2023-09-01 12:00:00",
      "2023-08-01 12:00:00"
    ),
    day = rep("Monday", 3),
    hour = rep(0L, 3),
    available = c(FALSE, TRUE, TRUE)
  )
  clean_tibble <- tibble::tibble(
    user_name = c("x", "y"),
    user_id = c("x1", "y1"),
    book_name = c("a", "b"),
    timezone = c("America/New_York", "Europe/Berlin"),
    datetime_utc = as.POSIXct(
      c(1698033600, 1698012000),
      tz = "UTC",
      origin = "1970-01-01"
    )
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
        test_result <- signups()
      },
      "Reading from x, sheet raw_clubs"
    ),
    "Downloading googledrive file x"
  )
  expect_identical(test_result, clean_tibble)
  memoise::forget(.cached_sheet_impl)
  memoise::forget(.rds_read)
})

test_that("Can fetch signups", {
  raw_sheet_tibble <- tibble::tibble(
    user_name = c("x", "x", "y"),
    user_id = c("x1", "x1", "y1"),
    book_name = c("a", "a", "b"),
    timezone = c("America/New_York", "America/New_York", "Europe/Berlin"),
    submission_timestamp = c(
      "2023-08-01 12:00:00",
      "2023-09-01 12:00:00",
      "2023-08-01 12:00:00"
    ),
    day = rep("Monday", 3),
    hour = rep(0L, 3),
    available = c(FALSE, TRUE, TRUE)
  )
  clean_tibble <- tibble::tibble(
    user_name = c("x", "y"),
    user_id = c("x1", "y1"),
    book_name = c("a", "b"),
    timezone = c("America/New_York", "Europe/Berlin"),
    datetime_utc = as.POSIXct(
      c(1698033600, 1698012000),
      tz = "UTC",
      origin = "1970-01-01"
    )
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
    {
      test_result <- signups()
    },
    "Downloading googledrive file x"
  )
  expect_identical(test_result, clean_tibble)
  memoise::forget(.cached_sheet_impl)
  memoise::forget(.rds_read)
})

test_that("Re-fetch memoisation works", {
  local_mocked_bindings(
    .rds_timestamp = function(...) 2,
    .rds_read = function(...) {
      message("Reading rds")
    },
    .googledrive_timestamp = function(...) 1
  )
  expect_message(
    {
      test_result <- signups()
    },
    "Reading rds"
  )
  expect_no_message({
    test_result <- signups()
  })
  expect_message(
    {
      test_result <- signups(refresh = TRUE)
    },
    "Reading rds"
  )
  memoise::forget(.cached_sheet_impl)
  memoise::forget(.rds_read)
})
