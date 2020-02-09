context("Exchangeable Arnold model")
use_seed <- 1632L

## #### Test implementation for d=2 ####
#
# Test that the implementation of the modified version of the Arnold model for
# exchangeable distributions works as intended for d = 2 and different choices
# for the ex_intensity vector.
test_that("Exchangeable Arnold model for d = 2", {
  n <- 25L # we use a default number of 25 simulations

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

  ## comonotone
  args[["ex_intensities"]] <- c(0, 1)
  expect_equal_sampling_result("rmo_ex_arnold", "test__rmo_ex_arnold_bivariate_R",
    args, n, use_seed)

  ## independence
  args[["ex_intensities"]] <- c(1, 0)
  expect_equal_sampling_result("rmo_ex_arnold", "test__rmo_ex_arnold_bivariate_R",
    args, n, use_seed)
})



## #### Test alternative implementation in `R` for d>2 ####
#
# Test that alternativ eimplementation based on the `a` parameters is equivalent
# for d = 5 and different choices for the ex_intensity vector.
test_that("Alternative implementation in R for d>2", {
  n <- 25L # we use a default number of 25 simulations

  ## all equal
  d <- 5L
  ex_intensities <- rep(1, times=d)
  args <- list("d" = d, "ex_intensities" = ex_intensities)
  expect_equal_sampling_result("rmo_ex_arnold", "test__rmo_ex_arnold_alternative_R",
    args, n, use_seed)

  ## heterogeneous ex_intensity vector
  args[["ex_intensities"]] <- c(0.4, 0.3, 0.2, 0.2, 0.1)
  expect_equal_sampling_result("rmo_ex_arnold", "test__rmo_ex_arnold_alternative_R",
    args, n, use_seed)

  ## comonotone
  args[["ex_intensities"]] <- c(0, 0, 0, 0, 1)
  expect_equal_sampling_result("rmo_ex_arnold", "test__rmo_ex_arnold_alternative_R",
    args, n, use_seed)

  ## independence
  args[["ex_intensities"]] <- c(1, 0, 0, 0, 0)
  expect_equal_sampling_result("rmo_ex_arnold", "test__rmo_ex_arnold_alternative_R",
    args, n, use_seed)
})


## #### Test implementation against original `R` version ####
#
# Test that the Rcpp implementation of the Arnold model delivers the same result
# as the original `R` implementation for various dimensions and choices of
# parameters.
test_that("Exchangeable Arnold model implementation in C++", {
  n <- 25L # we use a default number of 25 simulations

  # all equal
  d <- 7L
  ex_intensities <- rep(0.5, times=d)
  args <- list("d" = d, "ex_intensities" = ex_intensities)
  expect_equal_sampling_result("rmo_ex_arnold", "test__rmo_ex_arnold_R",
    args, n, use_seed)

  # d=4 + exchangeable
  args <-list("d" = 4L, "ex_intensities" = 1/(1:4L))
  d <- 4L
  ex_intensities <- 1/(1:4L)
  args <-list("d" = 4L, "ex_intensities" = ex_intensities)
  expect_equal_sampling_result("rmo_ex_arnold", "test__rmo_ex_arnold_R",
    args, n, use_seed)

  ## TODO: Implement tests based on the parametrisation with Bernstein
  ## functions to get realistic test-cases.
})
