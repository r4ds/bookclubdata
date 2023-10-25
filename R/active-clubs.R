#' Load Active Book Club Times
#'
#' Active book club times are stored in a Google Sheet, then parsed into an RDS.
#' This function loads the RDS if it is up-to-date, or updates the RDS if it is
#' out of date. Once this function has been called in a session, it always
#' returns the same result unless it is told to refresh.
#'
#' @param effective_date Pass a specific date to update the cached value.
#'
#' @return A tibble with columns `club`, `day_utc`, `hour_utc`, and `date_utc`.
#' @export
active_clubs_times <- function(effective_date = lubridate::today()) {
  active_clubs <- .cached_sheet_fetch(
    refresh = TRUE,
    rds_file_id = "1qvi47OCP14zruj8cnOR3AvlWUR-TBICc",
    googlesheet_id = "1xZbG0IMgvV1ySTA1dOvdM6GuikrHPhePJxYDqzTratA",
    read_args = list(sheet = "raw_clubs"),
    cleaner_fn = .active_clubs_clean
  )
  active_clubs$datetime_utc <- make_datetimes_utc(
    days = active_clubs$day_utc,
    hours = active_clubs$hour_utc,
    timezones = "UTC"
  )
  return(active_clubs)
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
  club_names <- stringr::str_extract(raw_names, "^[a-zA-Z0-9_]+")
  club_names[is.na(club_names)] <- "bad_club_name"
  return(club_names)
}
