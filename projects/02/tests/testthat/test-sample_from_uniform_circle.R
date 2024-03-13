# Load packages
library(testthat)
library(here)
library(sf)
library(dplyr)

# Source functions
source(here("R", "sample_from_uniform_circle.R"))

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

# Unit tests
## expect errors
test_that("arguments are of the right class", {
  # observations are sf dataframe
  expect_error(sample_from_uniform_circle(observations_sf3),
               regexp = "`observations` must be an sf object",
               fixed = TRUE)
  expect_error(sample_from_uniform_circle(observations = 2),
               regexp = "`observations` must be an sf object",
               fixed = TRUE)
  expect_error(sample_from_uniform_circle(observations = "string"),
               regexp = "`observations` must be an sf object",
               fixed = TRUE)

  # seed is numeric
  expect_error(sample_from_uniform_circle(observations_sf1, seed = "123"),
               regexp = "`seed` must be a numeric vector of length 1.",
               fixed = TRUE)
  expect_error(sample_from_uniform_circle(observations_sf2, seed = "123"),
               regexp = "`seed` must be a numeric vector of length 1.",
               fixed = TRUE)
})

test_that("arguments are of the right length", {
  # seed has length 1
  expect_error(sample_from_uniform_circle(observations_sf1, seed = 1:3),
               regexp = "`seed` must be a numeric vector of length 1.",
               fixed = TRUE)
  expect_error(sample_from_uniform_circle(observations_sf2, seed = 1:3),
               regexp = "`seed` must be a numeric vector of length 1.",
               fixed = TRUE)
})

## expect warnings
warning_message <- paste("No column `coordinateUncertaintyInMeters` present!",
                         "Assuming no uncertainty around observations.")
test_that("warning if coordinateUncertaintyInMeters column is not present", {
  expect_warning(sample_from_uniform_circle(observations_sf1),
                 regexp = warning_message,
                 fixed = TRUE)
})

## expected outputs
test_that("output class is correct", {
  suppressWarnings({
    expect_s3_class(sample_from_uniform_circle(observations_sf1),
                    class = "sf")
    expect_s3_class(sample_from_uniform_circle(observations_sf1),
                    class = "data.frame")
  })
  expect_s3_class(sample_from_uniform_circle(observations_sf2),
                  class = "sf")
  expect_s3_class(sample_from_uniform_circle(observations_sf2),
                  class = "data.frame")
})
