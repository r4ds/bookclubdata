set_state_inspector(function() {
  list(
    cached_sheet_impl = memoise::has_cache(.cached_sheet_impl)(),
    rds_read = memoise::has_cache(.rds_read)()
  )
})
