# I have a separate function for these in case I move where things are
# read/written.
.rds_timestamp <- function(rds_file) {
  .googledrive_timestamp(rds_file)
}

# I used this during creation, but don't call anywhere now. Keep for
# initialization, which might never be exported.
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
