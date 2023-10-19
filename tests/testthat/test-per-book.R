test_that("Can save signups for new book", {
  local_mocked_bindings(
    approved_books = function() {
      tibble::tibble(
        book_name = c("Has Data", "No Data"),
        book_abbr = c("has_data", "no_data")
      )
    },
    .signups_file_map = function(refresh) {
      tibble::tibble(
        book_abbr = c("has_data"),
        file_id = c("has_data_file_id")
      )
    },
    .rds_write = function(...) {
      cli::cli_inform("Writing new rds")
      return("no_data_file_id")
    },
    .rds_update = function(x, id) {
      cli::cli_inform("Updating rds {id}")
      return(id)
    }
  )
  expect_message(
    expect_message(
      expect_identical(
        signups_write("thing to write", "No Data"),
        "thing to write"
      ),
      "Writing new rds"
    ),
    "Updating rds 1W7htr2m-5_HN35R9Nk7xD6HEvijKJdrO"
  )
})

test_that("Can save signups to existing rds, new person", {
  user_times_df <- tibble::tibble(
    user_id = "new_person",
    data = "new data"
  )
  existing_times_df <- tibble::tibble(
    user_id = "existing_person",
    data = "old data"
  )
  local_mocked_bindings(
    approved_books = function() {
      tibble::tibble(
        book_name = c("Has Data", "No Data"),
        book_abbr = c("has_data", "no_data")
      )
    },
    .signups_file_map = function(refresh) {
      tibble::tibble(
        book_abbr = c("has_data"),
        file_id = c("has_data_file_id")
      )
    },
    .rds_write = function(...) {
      cli::cli_inform("Writing new rds")
      return("no_data_file_id")
    },
    .rds_read_uncached = function(file_id) {
      if (file_id == "has_data_file_id") {
        return(existing_times_df)
      } else {
        stop("unexpectd file_id")
      }
    },
    .rds_update = function(x, id) {
      cli::cli_inform("Updating rds {id}")
      return(id)
    }
  )
  expect_message(
    expect_identical(
      signups_write(user_times_df, "Has Data"),
      rbind(existing_times_df, user_times_df)
    ),
    "Updating rds has_data_file_id"
  )
})

test_that("Can save signups to existing rds, existing person", {
  user_times_df <- tibble::tibble(
    user_id = "existing_person",
    data = "new data"
  )
  existing_times_df <- tibble::tibble(
    user_id = c("existing_person", "other_person"),
    data = "old data"
  )
  local_mocked_bindings(
    approved_books = function() {
      tibble::tibble(
        book_name = c("Has Data", "No Data"),
        book_abbr = c("has_data", "no_data")
      )
    },
    .signups_file_map = function(refresh) {
      tibble::tibble(
        book_abbr = c("has_data"),
        file_id = c("has_data_file_id")
      )
    },
    .rds_write = function(...) {
      cli::cli_inform("Writing new rds")
      return("no_data_file_id")
    },
    .rds_read_uncached = function(file_id) {
      if (file_id == "has_data_file_id") {
        return(existing_times_df)
      } else {
        stop("unexpectd file_id")
      }
    },
    .rds_update = function(x, id) {
      cli::cli_inform("Updating rds {id}")
      return(id)
    }
  )
  expect_message(
    expect_identical(
      signups_write(user_times_df, "Has Data"),
      tibble::tibble(
        user_id = c("other_person", "existing_person"),
        data = c("old data", "new data")
      )
    ),
    "Updating rds has_data_file_id"
  )
})

test_that("Can clear existing signups for an existing person", {
  existing_times_df <- tibble::tibble(
    user_id = c("existing_person", "other_person"),
    data = "old data"
  )
  local_mocked_bindings(
    approved_books = function() {
      tibble::tibble(
        book_name = c("Has Data", "No Data"),
        book_abbr = c("has_data", "no_data")
      )
    },
    .signups_file_map = function(refresh) {
      tibble::tibble(
        book_abbr = c("has_data"),
        file_id = c("has_data_file_id")
      )
    },
    .rds_write = function(...) {
      stop("Should not write new.")
    },
    .rds_read_uncached = function(file_id) {
      if (file_id == "has_data_file_id") {
        return(existing_times_df)
      } else {
        stop("unexpectd file_id")
      }
    },
    .rds_update = function(x, id) {
      cli::cli_inform("Updating rds {id}")
      return(id)
    }
  )
  expect_message(
    expect_identical(
      signups_clear_user_book("existing_person", "Has Data"),
      tibble::tibble(
        user_id = c("other_person"),
        data = c("old data")
      )
    ),
    "Updating rds has_data_file_id"
  )
})

test_that("Can read existing signups", {
  existing_times_df <- tibble::tibble(
    user_id = c("existing_person", "other_person"),
    data = "old data"
  )
  local_mocked_bindings(
    approved_books = function() {
      tibble::tibble(
        book_name = c("Has Data", "No Data"),
        book_abbr = c("has_data", "no_data")
      )
    },
    .signups_file_map = function(refresh) {
      tibble::tibble(
        book_abbr = c("has_data"),
        file_id = c("has_data_file_id")
      )
    },
    .rds_write = function(...) {
      stop("Should not write.")
    },
    .rds_read_uncached = function(file_id) {
      stop("Should not read uncached.")
    },
    .rds_update = function(x, id) {
      stop("Should not write.")
    },
    .rds_read = function(file_id, refresh) {
      if (file_id == "has_data_file_id") {
        return(existing_times_df)
      } else {
        stop("unexpectd file_id")
      }
    }
  )
  expect_identical(signups_read("Has Data"), existing_times_df)
  expect_equal(nrow(signups_read("No Data")), 0)
})
