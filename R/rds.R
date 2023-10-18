# I have a separate function for these in case I change to something other than
# google drive.

.rds_timestamp <- function(rds_file) {
  .googledrive_timestamp(rds_file)
}

.rds_write <- function(x, name) {
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(x, path)
  return(.googledrive_upload_public(path, name = name))
}

.rds_update <- function(x, id) {
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(x, path)
  .googledrive_update(id, path)
}

.rds_read <- function(rds_file_id, refresh = TRUE) {
  if (refresh) {
    return(.rds_read_impl(rds_file_id, .rds_timestamp(rds_file_id)))
  }
  .rds_read_impl(rds_file_id)
}

.rds_read_impl <- function(rds_file_id, effective_date = NULL) {
  path <- withr::local_tempfile(fileext = ".rds")
  .googledrive_download(rds_file_id, path)
  return(readRDS(path))
}

.rds_read_uncached <- function(rds_file_id) {
  return(.rds_read_impl(rds_file_id, Sys.time()))
}
