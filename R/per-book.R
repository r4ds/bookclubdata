#' Read the signups rds for a book
#'
#' Read signups rds for a particular book.
#'
#' @param book_name The name of the book to read signups for.
#' @param refresh Whether to check for an update to the signups.
#'
#' @return A tibble with columns `user_name`, `user_id`, `timezone`, and
#'   `datetime_utc`.
#' @export
signups_read <- function(book_name, refresh = FALSE) {
  file_id <- .signups_file_id(book_name)
  if (length(file_id)) {
    signups <- .rds_read(file_id, refresh = refresh)
    signups$datetime_utc <- .update_datetime_utc(signups$datetime_utc)
    return(signups)
  }
  return(
    tibble::tibble(
      user_name = character(),
      user_id = character(),
      timezone = character(),
      datetime_utc = lubridate::as_datetime(integer())
    )
  )
}

.signups_file_id <- function(book_name) {
  abbr <- .book_abbr_fetch(book_name)
  signups_map <- .signups_file_map()
  file_id <- signups_map$file_id[signups_map$book_abbr == abbr]
  return(file_id)
}

.book_abbr_fetch <- function(book_name) {
  books <- approved_books()
  return(books$book_abbr[books$book_name == book_name])
}

.signups_file_map <- function(refresh = FALSE) {
  .rds_read("1W7htr2m-5_HN35R9Nk7xD6HEvijKJdrO", refresh) # nocov
}

#' Write the signups rds for a book
#'
#' Update or create the signups rds for a particular book.
#'
#' @inheritParams signups_read
#' @param user_times_df A tibble with columns `user_name`, `user_id`,
#'   `timezone`, and `datetime_utc`.
#'
#' @return The signups tibble for that book.
#' @export
signups_write <- function(user_times_df, book_name) {
  file_id <- .signups_file_id(book_name)
  if (length(file_id)) {
    return(.signups_update(user_times_df, file_id))
  }
  return(.signups_write_new(user_times_df, book_name))
}

#' Remove book signups for a user
#'
#' Clear all signups for a given user and book, and resave the associated rds.
#'
#' @inheritParams signups_read
#' @param user_id A string identifying the user.
#'
#' @return The signups tibble for that book.
#' @export
signups_clear_user_book <- function(user_id, book_name) {
  file_id <- .signups_file_id(book_name)
  signups <- .rds_read_uncached(file_id)
  signups <- signups[signups$user_id != user_id, ]
  .rds_update(signups, file_id)
  return(signups)
}

.signups_update <- function(user_times_df, file_id) {
  user_ids <- unique(user_times_df[["user_id"]])
  existing_data <- .rds_read_uncached(file_id)
  user_times_df <- rbind(
    existing_data[!(existing_data$user_id %in% user_ids), ],
    user_times_df
  )
  .rds_update(user_times_df, file_id)
  return(user_times_df)
}

.signups_write_new <- function(user_times_df, book_name) {
  abbr <- .book_abbr_fetch(book_name)
  filename <- paste0(abbr, ".rds")
  new_file_id <- .rds_write(user_times_df, filename)
  .signups_file_map_add(abbr, new_file_id)
  return(user_times_df)
}

.signups_file_map_add <- function(book_abbr, file_id) {
  signups_map <- .signups_file_map(refresh = TRUE)
  signups_map <- signups_map[signups_map$book_abbr != book_abbr, ]
  signups_map <- rbind(
    signups_map,
    tibble::tibble(book_abbr = book_abbr, file_id = file_id)
  )
  .rds_update(signups_map, "1W7htr2m-5_HN35R9Nk7xD6HEvijKJdrO")
}
