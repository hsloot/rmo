context("Arnold model")

use_seed <- 1632L

## Test that the implementation Arnold model
## works as expected for d = 2 and different choices
## for the intensity vector..

test_that("Arnold model implementation for d = 2", {
  n <- 25L

  ## all equal
  args <- list("d" = 2L, "intensities" = c(1, 1, 1))
  expect_equal_sampling_result("rmo_arnold", "test__rmo_arnold_bivariate_R",
                               args, n, use_seed)

  ## exchangeable, low, high
  args <- list("d" = 2L, "intensities" = c(0.1, 0.1, 2))
  expect_equal_sampling_result("rmo_arnold", "test__rmo_arnold_bivariate_R",
                               args, n, use_seed)

  ## exchangeable, high, low
  args <- list("d" = 2L, "intensities" = c(3, 3, 0.5))
  expect_equal_sampling_result("rmo_arnold", "test__rmo_arnold_bivariate_R",
                               args, n, use_seed)

  ## Low, High, Low
  args[["intensities"]] <- c(0.5, 3, 0.2)
  expect_equal_sampling_result("rmo_arnold", "test__rmo_arnold_bivariate_R",
                               args, n, use_seed)

  ## Low, Low, High
  args[["intensities"]] <- c(0.1, 0.005, 2)
  expect_equal_sampling_result("rmo_arnold", "test__rmo_arnold_bivariate_R",
                               args, n, use_seed)

  ## comonotone
  args[["intensities"]] <- c(0, 0, 1)
  expect_equal_sampling_result("rmo_arnold", "test__rmo_arnold_bivariate_R",
                               args, n, use_seed)

  ## independence
  args[["intensities"]] <- c(1, 1, 0)
  expect_equal_sampling_result("rmo_arnold", "test__rmo_arnold_bivariate_R",
                               args, n, use_seed)
})
