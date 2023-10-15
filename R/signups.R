.signups_rds_file_id <- "1V11PtVywfXS_Tya3wMFEaioOMuUDOJSS"
.signups_googlesheet_id <- "1G5KjY77ONuaHj530ttzrhCS9WN4_muYxfLgP3xK24Cc"

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
  if (refresh) {
    memoise::forget(.signups_impl)
  }
  return(.signups_impl())
}

.signups_impl <- function() {
  rds_timestamp <- .rds_timestamp(.signups_rds_file_id)
  sheet_timestamp <- .googledrive_timestamp(.signups_googlesheet_id)
  if (rds_timestamp < sheet_timestamp) {
    return(.signups_rds_update())
  }
  return(.signups_rds_read())
}

.signups_rds_update <- function() {
  signups <- .signups_sheet_read()
  signups <- .signups_clean(signups)
  return(.signups_rds_write(signups))
}

.signups_sheet_read <- function() {
  .googlesheet_read(
    .signups_googlesheet_id,
    sheet = "Signups",
    range = "A:H",
    col_types = "ccccccil"
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
  signups$submission_timestamp <- NULL
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

.signups_rds_write <- function(signups) {
  .rds_update(signups, .signups_rds_file_id)
  memoise::forget(.signups_rds_read)
  return(.signups_rds_read())
}

.signups_rds_read <- function() {
  path <- withr::local_tempfile(fileext = ".rds")
  .googledrive_download(.signups_rds_file_id, path)
  return(readRDS(path))
}
