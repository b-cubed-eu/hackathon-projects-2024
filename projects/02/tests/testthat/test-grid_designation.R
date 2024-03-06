# Source function to for observation to grid designation
source(here("R", "grid_designation.R"))

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("second multiplication works", {
  expect_equal(3 * 2, 5)
})

