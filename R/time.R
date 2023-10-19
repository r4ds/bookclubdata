#' Make datetimes next week
#'
#' Convert a vector of days, hours, and timezones to datetimes "next week" in
#' UTC. The dates are at least 7 days in the future.
#'
#' @param days A vector of English day names ("Monday", "Tuesday", etc).
#' @param hours A vector of hours.
#' @param timezones A vector of starting timezones.
#'
#' @return A vector of equivalent UTC times "next week."
#' @export
#'
#' @examples
#' make_datetimes_utc(
#'   days = c("Wednesday", "Saturday", "Thursday", "Monday"),
#'   hours = c(12L, 2L, 14L, 15L),
#'   timezones = c(
#'     "America/Chicago", "Europe/Rome", "Asia/Calcutta", "America/New_York"
#'   )
#' )
make_datetimes_utc <- function(days, hours, timezones) {
  datetimes <- .now()
  lubridate::second(datetimes) <- 0L
  lubridate::minute(datetimes) <- 0L
  lubridate::hour(datetimes) <- hours
  lubridate::wday(datetimes, week_start = 7) <- .days_to_int(days)
  datetimes <- .force_week_minimum(datetimes)
  lubridate::minute(datetimes) <- tz_minutes(timezones, datetimes)
  return(
    lubridate::force_tzs(
      datetimes,
      tzones = timezones,
      tzone_out = "UTC"
    )
  )
}

.days_to_int <- function(days) {
  as.integer(
    factor(
      days,
      levels = c(
        "Sunday", "Monday", "Tuesday", "Wednesday",
        "Thursday", "Friday", "Saturday"
      )
    )
  )
}

# Wrapped for test mocking.
.now <- function(tzone = "UTC") {
  lubridate::now(tzone = tzone) # nocov
}

#' Convert datetime minutes to 0 or 30
#'
#' Determine the minute offset of timezones from UTC on particular dates.
#'
#' @param timezones A character vector of timezones.
#' @param datetimes An optional vector of datetimes.
#'
#' @return A vector of 0L or 30L for each timezone.
#' @export
#'
#' @examples
#' tz_minutes(c("America/Chicago", "Europe/Rome", "Asia/Calcutta"))
tz_minutes <- function(timezones, datetimes = lubridate::now()) {
  lubridate::minute(datetimes) <- 0L
  return(lubridate::minute(lubridate::force_tzs(datetimes, tzones = timezones)))
}

.force_week_minimum <- function(datetimes) {
  while (any(datetimes - .now() < lubridate::weeks(1))) {
    too_small <- datetimes - .now() < lubridate::weeks(1)
    datetimes[too_small] <- datetimes[too_small] + lubridate::weeks(1)
  }
  return(datetimes)
}
