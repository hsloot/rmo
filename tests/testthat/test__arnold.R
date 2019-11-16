context("Arnold model")

use_seed <- 1632L

## Test that the implementation Arnold model
## works as expected for d = 2.

test_that("Arnold model implementation for d = 2", {
  n <- 100L

  args <- list("d" = 2L, "intensities" = c(0.5, 0.4, 0.2))
  expect_equal_sampling_result("rmo_arnold", "test__rmo_arnold_bivariate_R",
                               args, n, use_seed)
})
