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
