# Source functions
source(here("R", "sample_from_normal_circle.R"))

# Unit tests
test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("second multiplication works", {
  expect_equal(3 * 2, 5)
})
