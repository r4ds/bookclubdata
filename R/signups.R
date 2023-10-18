#' Process Signups
#'
#' Book club signups are logged in a Google Sheet, then parsed into an RDS. This
#' function loads the RDS if it is up-to-date, or updates the RDS if it is out
#' of date. Once this function has been called in a session, it always returns
#' the same result unless it is told to refresh.
#'
#' @param refresh Set this to `TRUE` to check for updates to the Google Sheet.
#'
#' @return A tibble with columns `user_name`, `user_id`, `book_name`,
#'   `timezone`, and `datetime_utc`.
#' @export
signups <- function(refresh = FALSE) {
  .cached_sheet_fetch(
    refresh = refresh,
    rds_file_id = "1V11PtVywfXS_Tya3wMFEaioOMuUDOJSS",
    googlesheet_id = "1G5KjY77ONuaHj530ttzrhCS9WN4_muYxfLgP3xK24Cc",
    read_args = list(sheet = "Signups", range = "A:H", col_types = "ccccccil"),
    cleaner_fn = .signups_clean
  )
}

.signups_clean <- function(signups) {
  signups <- .signups_distinct(signups)
  signups <- .signups_available(signups)
  signups <- .signups_times_utc(signups)
  return(signups)
}

.signups_distinct <- function(signups) {
  signups <- dplyr::distinct(
    dplyr::arrange(
      signups,
      dplyr::desc(lubridate::ymd_hms(.data$submission_timestamp))
    ),
    .data$user_id,
    .data$book_name,
    .data$day,
    .data$hour,
    .keep_all = TRUE
  )
  return(signups)
}

.signups_available <- function(signups) {
  signups <- dplyr::filter(signups, .data$available)
  return(signups)
}

.signups_times_utc <- function(signups) {
  signups <- signups |>
    dplyr::rowwise() |>
    dplyr::mutate(
      datetime_utc = .make_utc(
        day = .data$day,
        hour = .data$hour,
        timezone = .data$timezone
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-"day", -"hour", -"available")
  return(signups)
}
