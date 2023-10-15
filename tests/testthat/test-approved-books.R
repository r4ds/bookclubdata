test_that("Can update approved books", {
  raw_sheet_tibble <- tibble::tibble(
    book_url = c("https://y.com", "https://x.com"),
    book_name = c("Y", "x"),
    book_authors = c("a", "b"),
    book_copyright = list(NULL, lubridate::ymd("2020-01-01"))
  )
  clean_tibble <- tibble::tibble(
    book_url = c("https://x.com", "https://y.com"),
    book_name = c("x", "Y"),
    book_authors = c("b", "a"),
    book_copyright = c("2020-01-01", "")
  )
  local_mocked_bindings(
    .rds_timestamp = function(...) 1,
    .googledrive_download = function(file, path) {
      message("Downloading googledrive file x")
      saveRDS(clean_tibble, path)
    },
    .googledrive_timestamp = function(...) 2,
    .googlesheet_read = function(...) {
      message("Reading from x, sheet raw_clubs")
      raw_sheet_tibble
    },
    .rds_update = function(...) invisible()
  )
  expect_message(
    expect_message(
      {
        test_result <- approved_books()
      },
      "Reading from x, sheet raw_clubs"
    ),
    "Downloading googledrive file x"
  )
  expect_identical(test_result, clean_tibble)
  memoise::forget(.approved_books_impl)
  memoise::forget(.approved_books_rds_read)
})

test_that("Can fetch approved books", {
  raw_sheet_tibble <- tibble::tibble(
    book_url = c("https://y.com", "https://x.com"),
    book_name = c("Y", "x"),
    book_authors = c("a", "b"),
    book_copyright = list(NULL, lubridate::ymd("2020-01-01"))
  )
  clean_tibble <- tibble::tibble(
    book_url = c("https://x.com", "https://y.com"),
    book_name = c("x", "Y"),
    book_authors = c("b", "a"),
    book_copyright = c("2020-01-01", "")
  )
  local_mocked_bindings(
    .rds_timestamp = function(...) 2,
    .googledrive_download = function(file, path) {
      message("Downloading googledrive file x")
      saveRDS(clean_tibble, path)
    },
    .googledrive_timestamp = function(...) 1,
    .googlesheet_read = function(...) {
      message("Reading from x, sheet Clubs")
      raw_sheet_tibble
    }
  )
  expect_message(
    {test_result <- approved_books()},
    "Downloading googledrive file x"
  )
  expect_identical(test_result, clean_tibble)
  memoise::forget(.approved_books_impl)
  memoise::forget(.approved_books_rds_read)
})

test_that("Re-fetch memoisation works", {
  local_mocked_bindings(
    .rds_timestamp = function(...) 2,
    .approved_books_rds_read = function(...) {
      message("Reading rds")
    },
    .googledrive_timestamp = function(...) 1
  )
  expect_message(
    {test_result <- approved_books()},
    "Reading rds"
  )
  expect_no_message({test_result <- approved_books()})
  expect_message(
    {test_result <- approved_books(refresh = TRUE)},
    "Reading rds"
  )
  memoise::forget(.approved_books_impl)
  memoise::forget(.approved_books_rds_read)
})
