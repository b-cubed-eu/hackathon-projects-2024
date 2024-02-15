#' Observations to grid designation to create a data cube
#'
#' The function designates observations to cells of a given grid to create an aggregated data cube.
#'
#' @param observations An sf object with POINT geometry and a `coordinateUncertaintyInMeters` column. If this column is not present, the function will assume no (zero meters) uncertainty around the points.
#' @param grid An sf object with POLYGON geometry (usually a grid) to which observations should be designated
#' @param id_col The column name of the column with unique ids for each grid cell. If NULL (the default), a column `id` is created were the column numbers represent the unique ids.
#' @param seed The seed for random number generation to make results reproducible. If NULL (the default), no seed is used.
#'
#' @returns An sf object with the number of observations, geometry and minimal coordinate uncertainty per grid cell.
#'
#' @examples
#'
#' library(sf)
#'
#' n_points <- 4
#' xlim <- c(3841000, 3842000)
#' ylim <- c(3110000, 3112000)
#' coordinate_uncertainty <- rgamma(n_points, shape = 5, rate = 0.1)
#'
#' observations_sf <- data.frame(
#'   lat = runif(n_points, ylim[1], ylim[2]),
#'   long = runif(n_points, xlim[1], xlim[2]),
#'   coordinateUncertaintyInMeters = coordinate_uncertainty
#'   ) |>
#'   st_as_sf(coords = c("long", "lat"), crs = 3035)
#'
#' # Add buffer uncertainty in meters around points
#' observations_buffered <- observations_sf |>
#'   st_buffer(observations_sf$coordinateUncertaintyInMeters)
#'
#' # Create grid
#' grid_df <- st_make_grid(
#'   observations_buffered,
#'   square = TRUE,
#'   cellsize = c(200, 200)
#'   ) |>
#'   st_as_sf()
#'
#' # Create occurrence cube
#' grid_designation(
#'   observations = observations_sf,
#'   grid = grid_df,
#'   seed = 123)

grid_designation <- function(observations, grid, id_col = NULL, seed = NULL) {
  # Load packages
  stopifnot(requireNamespace("cli", quietly = TRUE))
  stopifnot(requireNamespace("dplyr", quietly = TRUE))
  stopifnot(requireNamespace("sf", quietly = TRUE))
  require(cli)
  require(dplyr)
  require(sf)

  # checks:
  # is id_col a character string or NULL
  # if present, does the id_col contain unique ids?
  # is seed an integer or NULL
  # crs of observations and grid needs to be the same
  # the input dataframes must be sf objects
  if (!"sf" %in% class(observations)) {
    cli::cli_abort(c(
      "{.var observations} must be an sf object",
      "x" = "You've supplied a {.cls {class(observations)}} object."
    ))
  }
  if (!"sf" %in% class(grid)) {
    cli::cli_abort(c(
      "{.var grid} must be an sf object.",
      "x" = "You've supplied a {.cls {class(grid)}} object.")
      )
  }



  # Set seed if provided
  if (!is.null(seed)) set.seed(seed)

  # Get random point in uncertainty circle
  if (!"coordinateUncertaintyInMeters" %in% names(observations)) {
    observations$coordinateUncertaintyInMeters <- 0
    cli::cli_warn(
      paste("No column `coordinateUncertaintyInMeters` present!",
            "Assuming no uncertainty around observations."))
  }

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
  sf::st_agr(new_points) <- "constant"
  sf::st_agr(grid) <- "constant"
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
