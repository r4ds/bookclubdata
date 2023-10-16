.onLoad <- function(libname, pkgname) { # nocov start
  .googledrive_authorize <<- memoise::memoise(.googledrive_authorize)
  .googlesheets_authorize <<- memoise::memoise(.googlesheets_authorize)
  .rds_read <<- memoise::memoise(.rds_read)
  .cached_sheet_impl <<- memoise::memoise(.cached_sheet_impl)
} # nocov end
