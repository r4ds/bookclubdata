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
  .cached_sheet_fetch(
    refresh = refresh,
    rds_file_id = "1qvi47OCP14zruj8cnOR3AvlWUR-TBICc",
    googlesheet_id = "1xZbG0IMgvV1ySTA1dOvdM6GuikrHPhePJxYDqzTratA",
    read_args = list(sheet = "raw_clubs"),
    cleaner_fn = .active_clubs_clean
  )
}

.active_clubs_clean <- function(active_clubs) {
  colnames(active_clubs) <- c(
    "club",
    "day_utc",
    "hour_utc"
  )
  active_clubs$club <- .active_clubs_clean_name(active_clubs$club)
  active_clubs$hour_utc <- as.integer(active_clubs$hour_utc)
  return(active_clubs)
}

.active_clubs_clean_name <- function(raw_names) {
  raw_names[raw_names == "(leave open for Project Club)"] <- "project_club"
  return(stringr::str_extract(raw_names, "^[a-zA-Z0-9_]+"))
}
