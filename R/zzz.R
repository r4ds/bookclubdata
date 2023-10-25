.onLoad <- function(libname, pkgname) { # nocov start
  .googledrive_authorize <<- memoise::memoise(.googledrive_authorize)
  .googlesheets_authorize <<- memoise::memoise(.googlesheets_authorize)
  .cached_sheet_impl <<- memoise::memoise(.cached_sheet_impl)
  active_clubs_times <<- memoise::memoise(active_clubs_times)
  .rds_read_impl <<- memoise::memoise(.rds_read_impl)
} # nocov end
