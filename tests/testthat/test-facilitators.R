test_that("Can update facilitator volunteers", {
  raw_sheet_tibble <- tibble::tibble(
    facilitator_id = c("A", "B"),
    facilitator_handle = c("A Handle", "B Handle"),
    book_abbr = c("book", "another")
  )
  local_mocked_bindings(
    .rds_timestamp = function(...) 1,
    .googledrive_download = function(file, path) {
      message("Downloading googledrive file x")
      saveRDS(raw_sheet_tibble, path)
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
        test_result <- facilitator_volunteers_read()
      },
      "Reading from x, sheet raw_clubs"
    ),
    "Downloading googledrive file x"
  )
  expect_identical(test_result, raw_sheet_tibble)
  memoise::forget(.cached_sheet_impl)
  memoise::forget(.rds_read_impl)
})

test_that("Can fetch facilitators", {
  raw_sheet_tibble <- tibble::tibble(
    facilitator_id = c("A", "B"),
    facilitator_handle = c("A Handle", "B Handle"),
    book_abbr = c("book", "another")
  )
  local_mocked_bindings(
    .rds_timestamp = function(...) 2,
    .googledrive_download = function(file, path) {
      message("Downloading googledrive file x")
      saveRDS(raw_sheet_tibble, path)
    },
    .googledrive_timestamp = function(...) 1,
    .googlesheet_read = function(...) {
      message("Reading from x, sheet Clubs")
      raw_sheet_tibble
    }
  )
  expect_message(
    {
      test_result <- facilitator_volunteers_read()
    },
    "Downloading googledrive file x"
  )
  expect_identical(test_result, raw_sheet_tibble)
  memoise::forget(.cached_sheet_impl)
  memoise::forget(.rds_read_impl)
})

test_that("Re-fetch memoisation works", {
  local_mocked_bindings(
    .rds_timestamp = function(...) 2,
    .rds_read = function(...) {
      message("Reading rds")
    },
    .googledrive_timestamp = function(...) 1
  )
  expect_message(
    {
      test_result <- facilitator_volunteers_read()
    },
    "Reading rds"
  )
  expect_no_message({
    test_result <- facilitator_volunteers_read()
  })
  expect_message(
    {
      test_result <- facilitator_volunteers_read(refresh = TRUE)
    },
    "Reading rds"
  )
  memoise::forget(.cached_sheet_impl)
  memoise::forget(.rds_read_impl)
})
