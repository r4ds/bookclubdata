# This is better, but it still updates any time someone submits a time (or
# whatever), rather than when the list of clubs changes. Ideally I should have
# an API that Google Sheets calls to update the RDS when the list of clubs
# actually changes.

.active_clubs_rds_file_id <- "1qvi47OCP14zruj8cnOR3AvlWUR-TBICc"
.clubs_googlesheet_id <- "1G5KjY77ONuaHj530ttzrhCS9WN4_muYxfLgP3xK24Cc"

#' Load Active Book Club Times
#'
#' Active book club times are stored in a Google Sheet, then parsed into an RDS.
#' This function loads the RDS if it is up-to-date, or updates the RDS if it is
#' out of date. Once this function has been called in a session, it always
#' returns the same result unless it is told to refresh.
#'
#' @param refresh Set this to `TRUE` to check for updates to the Google Sheet.
#'
#' @return A tibble with columns `club`, `day_utc`, `hour_utc`, `facilitator`,
#'   and `book_abbr`.
#' @export
active_clubs_times <- function(refresh = FALSE) {
  if (refresh) {
    memoise::forget(.active_clubs_times_impl)
  }
  return(.active_clubs_times_impl())
}

.active_clubs_times_impl <- function() {
  rds_timestamp <- .rds_timestamp(.active_clubs_rds_file_id)
  sheet_timestamp <- .googledrive_timestamp(.clubs_googlesheet_id)
  if (rds_timestamp < sheet_timestamp) {
    return(.active_clubs_rds_update())
  }
  return(.active_clubs_rds_read())
}

.active_clubs_rds_update <- function() {
  active_clubs <- .active_clubs_sheet_read()
  active_clubs <- .active_clubs_clean(active_clubs)
  return(.active_clubs_rds_write(active_clubs))
}

.active_clubs_sheet_read <- function() {
  .googlesheet_read(.clubs_googlesheet_id, sheet = "Clubs")
}

.active_clubs_clean <- function(active_clubs) {
  colnames(active_clubs) <- c(
    "club",
    "day_utc",
    "hour_utc",
    "cleanname",
    "facilitator",
    "book_abbr"
  )
  active_clubs$club <- active_clubs$cleanname
  active_clubs$cleanname <- NULL
  active_clubs$hour_utc <- as.integer(active_clubs$hour_utc)
  return(active_clubs)
}

.active_clubs_rds_write <- function(active_clubs) {
  .rds_update(active_clubs, .active_clubs_rds_file_id)
  memoise::forget(.active_clubs_rds_read)
  memoise::forget(.rds_timestamp)
  return(.active_clubs_rds_read())
}

.active_clubs_rds_read <- function() {
  path <- withr::local_tempfile(fileext = ".rds")
  .googledrive_download(.active_clubs_rds_file_id, path)
  return(readRDS(path))
}
