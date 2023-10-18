#' Load Approved Books
#'
#' Approved books are stored in a Google Sheet, then parsed into an RDS. This
#' function loads the RDS if it is up-to-date, or updates the RDS if it is out
#' of date. Once this function has been called in a session, it always returns
#' the same result unless it is told to refresh.
#'
#' @param refresh Set this to `TRUE` to check for updates to the Google Sheet.
#'
#' @return A tibble with columns `book_url`, `book_name`, `book_authors`, and
#'   `book_copyright`.
#' @export
approved_books <- function(refresh = FALSE) {
  .cached_sheet_fetch(
    refresh = refresh,
    rds_file_id = "15qpYCpaUTa9RvaWf4TCWmi4oGgbz58BZ",
    googlesheet_id = "19gm6rODTwU192cU_0vwAt8UQ9bU78OHgKKrd_3sCzA8",
    read_args = list(sheet = "approved", range = "B:F"),
    cleaner_fn = .approved_books_clean
  )
}

.approved_books_clean <- function(approved_books) {
  approved_books$book_copyright <- purrr::map_chr(
    approved_books$book_copyright,
    .approved_books_copyright_clean
  )
  return(approved_books[order(tolower(approved_books$book_name)), ])
}

.approved_books_copyright_clean <- function(x) {
  if (length(x)) {
    return(as.character(x))
  }
  return("")
}
