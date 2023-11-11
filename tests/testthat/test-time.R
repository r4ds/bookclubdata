test_that("Can make datetimes", {
  days <- c("Saturday", "Thursday", "Monday", "Monday")
  hours <- c(12L, 2L, 14L, 15L)
  timezones <- c(
    "America/Chicago",
    "Europe/Rome",
    "Asia/Calcutta",
    "America/New_York"
  )
  local_mocked_bindings(
    .now = function(tzone = "UTC") {
      lubridate::ymd_hms("2024-10-19 15:26:29", tz = tzone)
    }
  )
  expect_equal(
    make_datetimes_utc(days, hours, timezones),
    lubridate::ymd_hms(
      c(
        "2024-11-02 17:00:00",
        "2024-10-31 01:00:00",
        "2024-10-28 09:00:00",
        "2024-10-28 19:00:00"
      ),
      tz = "UTC"
    )
  )
})

test_that("Can update datetimes", {
  starting_datetimes <- c(
    lubridate::ymd_hms("2023-11-06 01:00:00 UTC"),
    lubridate::ymd_hms("2023-11-07 02:00:00 UTC")
  )
  updated_datetimes <- c(
    lubridate::ymd_hms("2024-10-28 01:00:00 UTC"),
    lubridate::ymd_hms("2024-10-29 02:00:00 UTC")
  )
  local_mocked_bindings(
    .now = function(tzone = "UTC") {
      lubridate::ymd_hms("2024-10-19 15:26:29", tz = tzone)
    }
  )
  expect_identical(.update_datetime_utc(starting_datetimes), updated_datetimes)
})

test_that("Empty datetimes don't cause errors", {
  starting_datetimes <- lubridate::POSIXct()
  local_mocked_bindings(
    .now = function(tzone = "UTC") {
      lubridate::ymd_hms("2024-10-19 15:26:29", tz = tzone)
    }
  )
  expect_identical(.update_datetime_utc(starting_datetimes), starting_datetimes)
})
