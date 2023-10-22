#' Load new facilitator volunteers
#'
#' Facilitator volunteers are loaded into a Google Sheet (eventually via a form,
#' perhaps), then parsed into an RDS. This function loads the RDS if it is
#' up-to-date, or updates the RDS if it is out of date. Once this function has
#' been called in a session, it always returns the same result unless it is told
#' to refresh.
#'
#' @param refresh Set this to `TRUE` to check for updates to the Google Sheet.
#'
#' @return A tibble with columns `facilitator_id`, `facilitator_handle`, and
#'   `book_abbr`.
#' @export
facilitator_volunteers_read <- function(refresh = FALSE) {
  .cached_sheet_fetch(
    refresh = refresh,
    rds_file_id = "1BtddWPooRU9Sia7Ztpf7pW42qOjJPMtw",
    googlesheet_id = "1i7C5sc5jovPLfZH1g8nWv3gW59bLZ568KcSELhKDH3k",
    read_args = list(sheet = "approved")
  )
}
