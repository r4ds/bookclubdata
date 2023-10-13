test_that(".rds_timestamp kinda works", {
  local_mocked_bindings(
    .googledrive_timestamp = function(file_id) 1
  )
  expect_identical(.rds_timestamp("x"), 1)
})

test_that(".rds_write works", {
  local_mocked_bindings(
    .googledrive_upload_public = function(path, name) {
      x <- readRDS(path)
      saveRDS(x, name)
    }
  )
  input <- sample(10)
  upload_path <- withr::local_tempfile(fileext = ".rds")
  .rds_write(input, upload_path)
  expect_identical(readRDS(upload_path), input)
})

test_that(".rds_update works", {
  local_mocked_bindings(
    .googledrive_update = function(file_id, path) {
      x <- readRDS(path)
      saveRDS(x, file_id)
    }
  )
  input <- sample(10)
  upload_path <- withr::local_tempfile(fileext = ".rds")
  .rds_update(input, upload_path)
  expect_identical(readRDS(upload_path), input)
})
