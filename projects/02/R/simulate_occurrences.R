#' Simulate occurrences within spatiotemporal scope
#'
#' The function simulates occurrences of a species within a given spatial and/or temporal extend.
#'
#' @param polygon
#' @param initial_average_abundance
#' @param spatial_autocorr
#' @param n_time_points
#' @param temporal_autocorr
#' @param spatiotemporal_autocorr
#' @param seed
#'
#' @returns An sf object with POINT geometry containing the locations of the simulated occurrences and a `time_point` column containing the time point associated with each occurrence.
#'
#' @examples
#'
#' # add example here

simulate_occurrences <- function(
    polygon,
    initial_average_abundance = 50,
    spatial_autocorr = c("random", "clustered", "regular"),
    n_time_points = 10,
    temporal_autocorr = ifelse(time_points ==  1, NA, "random_walk"),
    spatiotemporal_autocorr = NA,
    seed = NA) {
  # Load packages or install them if not available
  # (not good practise for package!)
  if (!requireNamespace("cli", quietly = TRUE)) install.packages("cli")
  if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
  if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
  require(cli)
  require(dplyr)
  require(sf)

  # ...
}
