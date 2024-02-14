grid_designation <- function(n_sim, observatins, grid, seed = 123, print = 100) {
  out <- vector(mode = "list", length = n_sim)

  set.seed(seed)
  for (i in 1:n_sim) {
    if (i %% print == 0) {
      message(paste0("* Generating cube ", i, "/", n_sim))
    }
    # Get random point
    test_points <-
      points %>%
      st_drop_geometry() %>%
      mutate(random_angle = runif(nrow(points), 0, 2*pi),
             random_r = sqrt(runif(nrow(points), 0, 1)) *
               coordinateUncertaintyInMeters)

    test_points2 <-
      test_points %>%
      mutate(x_new = x + random_r * cos(random_angle),
             y_new = y + random_r * sin(random_angle)) %>%
      st_as_sf(coords = c("x_new", "y_new"), crs = st_crs(31370))

    # We assign each occurrence to a grid cell
    intersect_grid <- st_intersection(test_points2, grid)

    # Aggregate to get the cube
    occ_cube <- intersect_grid %>%
      st_drop_geometry() %>%
      group_by(id) %>%
      summarize(n = n(),
                min_coord_uncertainty = min(coordinateUncertaintyInMeters)) %>%
      ungroup() %>%
      mutate(sim = i)

    out[[i]] <- occ_cube
  }

  final_occs <- do.call(rbind.data.frame, out)

  # Add zeroes
  design <- expand_grid(id = grid$id, sim = seq_len(n_sim))

  out_df <- final_occs %>%
    full_join(design, by = join_by(id, sim)) %>%
    mutate(n = ifelse(is.na(n), 0, n))

  return(out_df)
}