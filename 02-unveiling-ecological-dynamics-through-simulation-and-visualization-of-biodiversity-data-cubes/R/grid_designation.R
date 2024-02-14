# observations: sf object with geometry (POINT) and coordinateUncertaintyInMeters column
# grid: sf object with geometry (POLYGON)
# id_col
# seed: The seed for random number generation to make results reproducible. If NULL (the default), no seed is used.

grid_designation <- function(observations, grid, id_col = NULL, seed = NULL) {
  # Load packages
  stopifnot(requireNamespace("dplyr", quietly = TRUE))
  stopifnot(requireNamespace("sf", quietly = TRUE))
  require(dplyr)
  require(sf)

  # checks: crs of observations and grid needs to be the same


  # Set seed if provided
  if (!is.null(seed)) set.seed(seed)

  # Get random point in uncertainty circle
  uncertainty_points <-
    observations |>
    dplyr::mutate(
      random_angle = runif(nrow(observations), 0, 2 * pi),
      random_r = sqrt(runif(nrow(observations), 0, 1)) *
        coordinateUncertaintyInMeters)

  new_points <-
    uncertainty_points |>
    dplyr::mutate(
      x_new = sf::st_coordinates(geometry)[, 1] +
        random_r * cos(random_angle),
      y_new = sf::st_coordinates(geometry)[, 2] +
        random_r * sin(random_angle)) |>
    sf::st_as_sf(coords = c("x_new", "y_new"), crs = sf::st_crs(observations))

  # We assign each occurrence to a grid cell
  if (is.null(id_col)) {
    id_col <- "id"
    grid[[id_col]] <- rownames(grid)
  }
  intersect_grid <- sf::st_intersection(new_points, grid)

  # Aggregate to get the cube
  occ_cube_df <- intersect_grid |>
    sf::st_drop_geometry() |>
    dplyr::group_by_at(id_col) |>
    dplyr::summarise(
      n = dplyr::n(),
      min_coord_uncertainty = min(coordinateUncertaintyInMeters)) |>
    dplyr::ungroup()

  # Add zeroes
  out_sf <- occ_cube_df |>
    dplyr::full_join(grid, by = dplyr::join_by(!!id_col)) |>
    dplyr::mutate(n = as.integer(ifelse(is.na(n), 0, n))) %>%
    sf::st_as_sf(crs = sf::st_crs(grid))

  return(out_sf)
}
