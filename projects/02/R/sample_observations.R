#' Sample observations from a larger occurrence dataset
#'
#' The function samples observations from occurrences based on detection probability and sampling bias.
#'
#' @param occurrences
#' @param detection_probability
#' @param sampling_bias
#' @param bias_area
#' @param bias_strength
#' @param bias_weights
#' @param coordinate_uncertainty_meters
#' @param seed
#'
#' @returns An sf object with POINT geometry containing the locations of the simulated occurrences and a `time_point` column containing the time point associated with each occurrence.
#'
#' @examples
#'
#' # add example here

sample_observations <- function(
    occurrences,
    detection_probability = 1,
    sampling_bias = c("no_bias", "polygon", "manual"),
    bias_area = NA,
    bias_strength = NA,
    bias_weights = NA,
    coordinate_uncertainty_meters = 25,
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
