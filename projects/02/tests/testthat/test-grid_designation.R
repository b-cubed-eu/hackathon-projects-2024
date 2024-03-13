# Load packages
library(testthat)
library(here)
library(sf)
library(dplyr)

# Source functions
source(here("R", "grid_designation.R"))

# Prepare example datasets
## number of points and extend
n_points <- 4
xlim <- c(3841000, 3842000)
ylim <- c(3110000, 3112000)

## dataset without coordinateUncertaintyInMeters
observations_sf1 <- data.frame(
  lat = runif(n_points, ylim[1], ylim[2]),
  long = runif(n_points, xlim[1], xlim[2])
  ) %>%
  st_as_sf(coords = c("long", "lat"), crs = 3035)

## dataset with coordinateUncertaintyInMeters
set.seed(123)
coordinate_uncertainty <- rgamma(n_points, shape = 5, rate = 0.1)
observations_sf2 <- observations_sf1 %>%
  mutate(coordinateUncertaintyInMeters = coordinate_uncertainty)

# Add buffer uncertainty in meters around points
observations_buffered <- observations_sf2 %>%
  st_buffer(observations_sf2$coordinateUncertaintyInMeters)

# Create grid
grid_df1 <- st_make_grid(
  observations_buffered,
  square = TRUE,
  cellsize = c(200, 200)
  ) %>%
  st_as_sf()

## grid without geometry
grid_df3 <- st_make_grid(
  observations_buffered,
  square = TRUE,
  cellsize = c(200, 200)
  ) %>%
  st_as_sf() %>%
  st_drop_geometry()

# Unit tests
## expect errors
test_that("arguments are of the right class", {
  # grid is sf dataframe
  expect_error(grid_designation(observations_sf2, grid_df3),
              regexp = "`grid` must be an sf object",
              fixed = TRUE)
  expect_error(grid_designation(observations_sf2, grid = 2),
              regexp = "`grid` must be an sf object",
              fixed = TRUE)
  expect_error(grid_designation(observations_sf2, grid = "string"),
              regexp = "`grid` must be an sf object",
              fixed = TRUE)

  # seed is numeric
})

test_that("arguments are of the right length", {
  # seed has length 1
  expect_error(grid_designation(observations_sf1, seed = 1:3),
              regexp = "`seed` must be a numeric vector of length 1.",
              fixed = TRUE)
  expect_error(grid_designation(observations_sf2, seed = 1:3),
              regexp = "`seed` must be a numeric vector of length 1.",
              fixed = TRUE)
 })

## expect warnings


## expected outputs

