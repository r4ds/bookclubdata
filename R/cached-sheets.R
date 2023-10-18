.cached_sheet_fetch <- function(refresh = FALSE,
                                rds_file_id,
                                googlesheet_id,
                                read_args = list(),
                                cleaner_fn = NULL) {
  if (refresh) {
    .cached_sheet_drop_cache(
      rds_file_id = rds_file_id,
      googlesheet_id = googlesheet_id,
      read_args = read_args,
      cleaner_fn = cleaner_fn
    )
  }
  return(
    .cached_sheet_impl(
      rds_file_id = rds_file_id,
      googlesheet_id = googlesheet_id,
      read_args = read_args,
      cleaner_fn = cleaner_fn
    )
  )
}

.cached_sheet_impl <- function(rds_file_id,
                               googlesheet_id,
                               read_args = list(),
                               cleaner_fn = NULL) {
  rds_timestamp <- .rds_timestamp(rds_file_id)
  sheet_timestamp <- .googledrive_timestamp(googlesheet_id)
  if (rds_timestamp < sheet_timestamp) {
    return(
      .cached_sheet_rds_update(
        rds_file_id,
        googlesheet_id,
        read_args,
        cleaner_fn
      )
    )
  }
  return(.rds_read(rds_file_id))
}

.cached_sheet_drop_cache <- function(rds_file_id,
                                     googlesheet_id,
                                     read_args = list(),
                                     cleaner_fn = NULL) {
  memoise::drop_cache(.cached_sheet_impl)(
    rds_file_id,
    googlesheet_id,
    read_args = read_args,
    cleaner_fn = cleaner_fn
  )
}

.cached_sheet_rds_update <- function(rds_file_id,
                                     googlesheet_id,
                                     read_args,
                                     cleaner_fn) {
  cached_sheet <- rlang::inject(
    .googlesheet_read(googlesheet_id, !!!read_args)
  )
  if (!is.null(cleaner_fn)) {
    cached_sheet <- cleaner_fn(cached_sheet)
  }
  return(.cached_sheet_rds_write(cached_sheet, rds_file_id))
}

.cached_sheet_rds_write <- function(cached_sheet, rds_file_id) {
  .rds_update(cached_sheet, rds_file_id)
  return(.rds_read(rds_file_id))
}
