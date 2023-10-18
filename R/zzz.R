.onLoad <- function(libname, pkgname) { # nocov start
  .googledrive_authorize <<- memoise::memoise(.googledrive_authorize)
  .googlesheets_authorize <<- memoise::memoise(.googlesheets_authorize)
  .cached_sheet_impl <<- memoise::memoise(.cached_sheet_impl)
  .rds_read_impl <<- memoise::memoise(.rds_read_impl)
} # nocov end
