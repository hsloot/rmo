context("Exchangeable Arnold model")
use_seed <- 1632L

## Test that the implementation of the modified version of the
## Arnold model for exchangeable distributions works as intended
## for d = 2 and different choices for the ex_intensity vector.
test_that("Exchangeable Arnold model for d = 2", {
  n <- 25L

  ## all equal
  args <- list("d" = 2L, ex_intensities = c(1, 1))
  expect_equal_sampling_result("rmo_ex_arnold", "test__rmo_ex_arnold_bivariate_R",
                               args, n, use_seed)

  ## low, high
  args[["ex_intensities"]] <- c(0.5, 2)
  expect_equal_sampling_result("rmo_ex_arnold", "test__rmo_ex_arnold_bivariate_R",
                               args, n, use_seed)

  ## high, low
  args[["ex_intensities"]] <- c(3, 0.2)
  expect_equal_sampling_result("rmo_ex_arnold", "test__rmo_ex_arnold_bivariate_R",
                               args, n, use_seed)
})



## Test that alternativ eimplementation based on the `a`
## parameters is equivalent for d = 5 and different choices
## for the ex_intensity vector..
test_that("Alternative implementation", {
  n <- 100L

  ## all equal
  args <- list("d" = 5L, ex_intensities = rep(1, 5L))
  expect_equal_sampling_result("rmo_ex_arnold", "test__rmo_ex_arnold_alternative_R",
                               args, n, use_seed)

  ## heterogeneous ex_intensity vector
  args <- list("d" = 5L, ex_intensities = c(0.4, 0.3, 0.2, 0.2, 0.1))
  expect_equal_sampling_result("rmo_ex_arnold", "test__rmo_ex_arnold_alternative_R",
                               args, n, use_seed)
})
