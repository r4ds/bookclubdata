set_state_inspector(function() {
  list(
    ac_rds_read = memoise::has_cache(.active_clubs_rds_read)(),
    ac_times_impl = memoise::has_cache(.active_clubs_times_impl)(),
    rds_timestamp = memoise::has_cache(.rds_timestamp)(
      .active_clubs_rds_file_id
    )
  )
})
