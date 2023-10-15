.approved_books_rds_file_id <- "15qpYCpaUTa9RvaWf4TCWmi4oGgbz58BZ"
.approved_books_googlesheet_id <- "19gm6rODTwU192cU_0vwAt8UQ9bU78OHgKKrd_3sCzA8"

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
  if (refresh) {
    memoise::forget(.approved_books_impl)
  }
  return(.approved_books_impl())
}

.approved_books_impl <- function() {
  rds_timestamp <- .rds_timestamp(.approved_books_rds_file_id)
  sheet_timestamp <- .googledrive_timestamp(.approved_books_googlesheet_id)
  if (rds_timestamp < sheet_timestamp) {
    return(.approved_books_rds_update())
  }
  return(.approved_books_rds_read())
}

.approved_books_rds_update <- function() {
  approved_books <- .approved_books_sheet_read()
  approved_books <- .approved_books_clean(approved_books)
  return(.approved_books_rds_write(approved_books))
}

.approved_books_sheet_read <- function() {
  .googlesheet_read(
    .approved_books_googlesheet_id,
    sheet = "approved",
    range = "B:E"
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

.approved_books_rds_write <- function(approved_books) {
  .rds_update(approved_books, .approved_books_rds_file_id)
  memoise::forget(.approved_books_rds_read)
  return(.approved_books_rds_read())
}

.approved_books_rds_read <- function() {
  path <- withr::local_tempfile(fileext = ".rds")
  .googledrive_download(.approved_books_rds_file_id, path)
  return(readRDS(path))
}
