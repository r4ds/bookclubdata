.onLoad <- function(libname, pkgname) { # nocov start
  .googledrive_authorize <<- memoise::memoise(.googledrive_authorize)
  .googlesheets_authorize <<- memoise::memoise(.googlesheets_authorize)
  .active_clubs_rds_read <<- memoise::memoise(.active_clubs_rds_read)
  .active_clubs_times_impl <<- memoise::memoise(.active_clubs_times_impl)
} # nocov end
