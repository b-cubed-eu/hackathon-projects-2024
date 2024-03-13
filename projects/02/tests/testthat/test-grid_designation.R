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

## dataset without geometry
observations_sf3 <- observations_sf2 %>%
  st_drop_geometry()

# Add buffer uncertainty in meters around points
observations_buffered <- observations_sf2 %>%
  st_buffer(observations_sf2$coordinateUncertaintyInMeters)

# Create grid
grid_df1 <- st_make_grid(
  observations_buffered,
  square = TRUE,
  cellsize = c(200, 200)
  ) %>%
  st_as_sf() %>%
  rename(geometry = x)

## grid without geometry
grid_df3 <- grid_df1 %>%
  st_drop_geometry()

# Unit tests
## expect errors
test_that("arguments are of the right class", {
  # observations are sf dataframe
  expect_error(grid_designation(observations_sf3, grid_df1),
               regexp = "`observations` must be an sf object",
               fixed = TRUE)
  expect_error(grid_designation(observations = 2, grid_df1),
               regexp = "`observations` must be an sf object",
               fixed = TRUE)
  expect_error(grid_designation(observations = "string", grid_df1),
               regexp = "`observations` must be an sf object",
               fixed = TRUE)

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

  # id_col is string
  expect_error(grid_designation(observations_sf2, grid = grid_df1,
                                id_col = 3),
               regexp = "`id_col` must be a character vector of length 1.",
               fixed = TRUE)

  # randomisation is string
  expect_error(grid_designation(observations_sf2, grid = grid_df1,
                                randomisation = 3),
               regexp = "`randomisation` must be a character vector.",
               fixed = TRUE)

  # aggregate is logical
  expect_error(grid_designation(observations_sf2, grid = grid_df1,
                                aggregate = "TRUE"),
               regexp = "`aggregate` must be a logical vector of length 1.",
               fixed = TRUE)
})

test_that("arguments are of the right length", {
  # id_col has length 1
  expect_error(grid_designation(observations_sf2, grid = grid_df1,
                                id_col = c("col1", "col2")),
               regexp = "`id_col` must be a character vector of length 1.",
               fixed = TRUE)

  # aggregate has length 1
  expect_error(grid_designation(observations_sf2, grid = grid_df1,
                                aggregate = rep(TRUE, 3)),
               regexp = "`aggregate` must be a logical vector of length 1.",
               fixed = TRUE)
})

test_that("crs of observations and grid must match", {
  expect_error(
    grid_designation(observations_sf2,
                     grid = st_transform(grid_df1, crs = 4326)),
    regexp = "sf::st_crs(observations) == sf::st_crs(grid) is not TRUE",
    fixed = TRUE)
})

test_that('randomisation should be one of "uniform", "normal"', {
  expect_error(
    grid_designation(observations_sf2, grid_df1, randomisation = "beta"),
    regexp = '`randomisation` should be one of "uniform", "normal".',
    fixed = TRUE)
})

## expect warnings
test_that("unique ids if id column is provided", {
  expect_warning(
    grid_designation(observations_sf2,
                     grid = grid_df1 %>%
                       mutate(id = 1),
                     id_col = "id"),
    regexp = "Column `id` does not contain unique ids for grid cells!",
    fixed = TRUE)
})

test_that("provided id column present in provided grid", {
  expect_warning(
    grid_designation(observations_sf2,
                     grid = grid_df1 %>%
                       mutate(id = seq_len(nrow(grid_df1))),
                     id_col = "identifier"),
    regexp = 'Column name "identifier" not present in provided grid!',
    fixed = TRUE)
})

## expected outputs
test_that("output class is correct", {
  # Aggregate = TRUE
  suppressWarnings({
    expect_s3_class(grid_designation(observations_sf1, grid = grid_df1),
                    class = "sf")
    expect_s3_class(grid_designation(observations_sf1, grid = grid_df1),
                    class = "data.frame")
  })
  expect_s3_class(grid_designation(observations_sf2, grid = grid_df1),
                  class = "sf")
  expect_s3_class(grid_designation(observations_sf2, grid = grid_df1),
                  class = "data.frame")

  # Aggregate = FALSE
  suppressWarnings({
    expect_s3_class(grid_designation(observations_sf1, grid = grid_df1,
                                     aggregate = FALSE),
                    class = "sf")
    expect_s3_class(grid_designation(observations_sf1, grid = grid_df1,
                                     aggregate = FALSE),
                    class = "data.frame")
  })
  expect_s3_class(grid_designation(observations_sf2, grid = grid_df1,
                                   aggregate = FALSE),
                  class = "sf")
  expect_s3_class(grid_designation(observations_sf2, grid = grid_df1,
                                   aggregate = FALSE),
                  class = "data.frame")
})

test_that("correct column names present", {
  # Aggregate = TRUE
  suppressWarnings({
    expect_contains(names(grid_designation(observations_sf1, grid = grid_df1)),
                    c("id", "n", "min_coord_uncertainty", "geometry"))
  })
  expect_contains(names(grid_designation(observations_sf2, grid = grid_df1)),
                  c("id", "n", "min_coord_uncertainty", "geometry"))
  expect_contains(
    names(grid_designation(
      observations_sf2,
      grid = grid_df1 %>%
        mutate(identifier = seq_len(nrow(grid_df1))),
      id_col = "identifier")),
    c("identifier", "n", "min_coord_uncertainty", "geometry"))

  # Aggregate = FALSE
  suppressWarnings({
    expect_contains(names(grid_designation(observations_sf1, grid = grid_df1,
                                           aggregate = FALSE)),
                    c("id", "coordinateUncertaintyInMeters", "geometry"))
  })
  expect_contains(names(grid_designation(observations_sf2, grid = grid_df1,
                                         aggregate = FALSE)),
                  c("id", "coordinateUncertaintyInMeters", "geometry"))
  expect_contains(
    names(grid_designation(
      observations_sf2,
      grid = grid_df1 %>%
        mutate(identifier = seq_len(nrow(grid_df1))),
      id_col = "identifier",
      aggregate = FALSE)),
    c("identifier", "coordinateUncertaintyInMeters", "geometry"))
})
