#' Observations to grid designation to create a data cube
#'
#' The function designates observations to cells of a given grid to create an aggregated data cube.
#'
#' @param observations An sf object with POINT geometry and a `coordinateUncertaintyInMeters` column. If this column is not present, the function will assume no (zero meters) uncertainty around the points.
#' @param grid An sf object with POLYGON geometry (usually a grid) to which observations should be designated
#' @param id_col The column name of the column with unique ids for each grid cell. If NULL (the default), a column `id` is created were the column numbers represent the unique ids.
#' @param seed The seed for random number generation to make results reproducible. If NULL (the default), no seed is used.
#' @param aggregate Logical. If TRUE (default), return data cube in aggregated form (grid with number of observations per grid cell). Otherwise return sampled points in uncertainty circle.
#' @param randomisation Randomisation method used for sampling within uncertainty circle around each observation. By default "uniform" which means each point uncertainty circle has an equal probability to be selected. The other option is "normal" where a point is sampled from a bivariate Normal distribution with means equal to the observation point and the variance equal to (-`coordinateUncertaintyInMeters`^2) / (2 * log(1 - `p_norm`)) such that `p_norm`% of all possible samples from this Normal distribution fall within the uncertainty circle.
#' @param p_norm Not applicable for uniform randomisation. The proportion of all possible samples from a a bivariate Normal distribution that fall within the uncertainty circle. If normal randomisation is used and no value is given, the default p_norm value is 0.95.
#'
#' @returns In case of `aggregate = TRUE` an sf object with the number of observations, geometry and minimal coordinate uncertainty per grid cell. In case of `aggregate = TRUE` an sf object with the geometry of the resampled observations in the uncertainty circle and the coordinate uncertainty in meters related to the original observation.
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

grid_designation <- function(
    observations,
    grid,
    id_col = NULL,
    seed = NULL,
    aggregate = TRUE,
    randomisation = "uniform",
    p_norm = NA) {
  # Load packages or install them if not available
  if (!requireNamespace("cli", quietly = TRUE)) install.packages("cli")
  if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
  if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
  require(cli)
  require(dplyr)
  require(sf)

  # Checks:
  # the input dataframes must be sf objects
  if (!"sf" %in% class(observations)) {
    cli::cli_abort(c(
      "{.var observations} must be an sf object",
      "x" = "You've supplied a {.cls {class(observations)}} object.")
      )
  }
  if (!"sf" %in% class(grid)) {
    cli::cli_abort(c(
      "{.var grid} must be an sf object.",
      "x" = "You've supplied a {.cls {class(grid)}} object.")
      )
  }
  # crs of observations and grid needs to be the same
  if (sf::st_crs(observations) != sf::st_crs(grid)) {
    cli::cli_abort("sf::st_crs(observations) == sf::st_crs(grid) is not TRUE")
  }
  # unique ids if id column is provided
  if (!is.null(id_col)) {
    if (!id_col %in% names(grid)) {
      cli::cli_warn(
        paste('Column name "{id_col}" not present in provided grid!',
              "Creating ids based on row numbers.")
        )
      id_col <- NULL
    } else if (length(unique(grid[[id_col]])) != nrow(grid)) {
      cli::cli_warn(
        paste("Column `{id_col}` does not contain unique ids for grid",
              "cells! Creating new ids based on row numbers.")
        )
      id_col <- NULL
    }
  }

  # Set seed if provided
  if (!is.null(seed)) {
    if (is.numeric(seed) & length(seed) == 1) {
      set.seed(seed)
    } else {
      cli::cli_abort(c(
        "{.var seed} must be an numeric vector of length 1.",
        "x" = paste("You've supplied a {.cls {class(seed)}} vector",
                    "of length {length(seed)}."))
        )
    }
  }

  # Set uncertainty to zero if column not present in data
  if (!"coordinateUncertaintyInMeters" %in% names(observations)) {
    observations$coordinateUncertaintyInMeters <- 0
    cli::cli_warn(
      paste("No column {.var coordinateUncertaintyInMeters} present!",
            "Assuming no uncertainty around observations.")
      )
  }

  # Get random point in uncertainty circle according to uniform or normal rules
  if (randomisation == "uniform") {
    # Get random angle and radius
    uncertainty_points <-
      observations |>
      dplyr::mutate(
        random_angle = runif(nrow(observations), 0, 2 * pi),
        random_r = sqrt(runif(nrow(observations), 0, 1)) *
          coordinateUncertaintyInMeters)

    # Calculate new point
    new_points <-
      uncertainty_points |>
      dplyr::mutate(
        x_new = sf::st_coordinates(geometry)[, 1] +
          random_r * cos(random_angle),
        y_new = sf::st_coordinates(geometry)[, 2] +
          random_r * sin(random_angle)) |>
      sf::st_drop_geometry() |>
      sf::st_as_sf(coords = c("x_new", "y_new"), crs = sf::st_crs(observations))
  } else if (randomisation == "normal") {
    # Set up probability of inclusion
    if (is.na(p_norm)) {
      p_norm <- 0.95
    }
    # Should be a single value between 0 and 1
    if (length(p_norm) != 1 || p_norm <= 0 || p_norm >= 1) {
      if (is.numeric(p_norm)) {
        cli::cli_abort(c(
          "{.var p_norm} must be a single value between 0 and 1.",
          "x" = paste("You've supplied the value(s) {p_norm}."))
          )
      } else {
        cli::cli_abort(c(
          "{.var p_norm} must be a numeric vector of length 1 .",
          "x" = paste("You've supplied a {.cls {class(p_norm)}} vector",
                      "of length {length(p_norm)}."))
          )
      }
    }

    # Package to sample from bivariate Normal distribution
    if (!requireNamespace("mnormt", quietly = TRUE)) install.packages("mnormt")
    require(mnormt)

    # Calculate 2-dimensional means and variance-covariance matrices
    means <- sf::st_coordinates(observations$geometry)
    variances <- (-observations$coordinateUncertaintyInMeters^2) /
      (2 * log(1 - p_norm))
    varcovariances <- lapply(variances, function(var) {
      matrix(c(var, -1, -1, var), nrow = 2)
      })

    # Sample new points from bivariate Normal distribution
    new_points_list <- vector("list", length = nrow(observations))
    for (i in seq_len(nrow(observations))) {
      new_points_list[[i]] <- rmnorm(
        1, mean = means[i,], varcov = varcovariances[[i]]
        )
    }
    new_points_df <- do.call(rbind.data.frame, new_points_list)
    names(new_points_df) <- c("x_new", "y_new")

    # Create geometry and add uncertainties
    new_points <- cbind(
      new_points_df,
      coordinateUncertaintyInMeters = observations$coordinateUncertaintyInMeters
      ) |>
      sf::st_as_sf(coords = c("x_new", "y_new"), crs = sf::st_crs(observations))
  } else {
    cli::cli_abort(c(
      '{.var randomisation} must be "uniform" or "normal".',
      "x" = 'You provided "{randomisation}".')
      )
  }

  # We assign each occurrence to a grid cell
  # Each grid cell needs a unique id
  if (is.null(id_col)) {
    id_col <- "id"
    grid[[id_col]] <- rownames(grid)
  }
  sf::st_agr(new_points) <- "constant"
  sf::st_agr(grid) <- "constant"
  intersect_grid <- sf::st_intersection(new_points, grid)

  if (isTRUE(aggregate)) {
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
  } else if (isFALSE(aggregate)) {
    out_sf <- intersect_grid |>
      dplyr::select_at(c(id_col, "coordinateUncertaintyInMeters"))
  } else {
    cli::cli_abort(c(
      "{.var aggregate} must be logical.",
      "x" = paste("You've supplied an object of class
                  {.cls {class(aggregate)}}."))
    )
  }

  return(out_sf)
}
