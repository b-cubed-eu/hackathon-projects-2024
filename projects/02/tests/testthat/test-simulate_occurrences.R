# Source functions
source(here("R", "simulate_occurrences.R"))

# Unit tests
test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("second multiplication works", {
  expect_equal(3 * 2, 5)
})
