set_state_inspector(function() {
  list(
    active_rds_read = memoise::has_cache(.active_clubs_rds_read)(),
    active_times_impl = memoise::has_cache(.active_clubs_times_impl)(),
    approved_rds_read = memoise::has_cache(.approved_books_rds_read)(),
    approved_times_impl = memoise::has_cache(.approved_books_impl)()
  )
})
