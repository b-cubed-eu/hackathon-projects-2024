# Source functions
source(here("R", "sample_observations.R"))

# Unit tests
test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
