context("Arnold model")
use_seed <- 1632L
n <- 100L

## #### Test implementation for the bivariate case ####
#
# Test that the implementation Arnold model works as expected the bivariate
# case and different choices for the intensity vector.

test_that("Arnold model implementation for d = 2", {
  ## all equal
  args <- list("d" = 2L, "intensities" = c(1, 1, 1))
  expect_equal_rn_generation(
    "rmo_arnold", "test__rmo_arnold_bivariate_R",
    args, n, use_seed)

  ## exchangeable, low, high
  args <- list("d" = 2L, "intensities" = c(0.1, 0.1, 2))
  expect_equal_rn_generation(
    "rmo_arnold", "test__rmo_arnold_bivariate_R",
    args, n, use_seed)

  ## exchangeable, high, low
  args <- list("d" = 2L, "intensities" = c(3, 3, 0.5))
  expect_equal_rn_generation(
    "rmo_arnold", "test__rmo_arnold_bivariate_R",
    args, n, use_seed)

  ## Low, High, Low
  args[["intensities"]] <- c(0.5, 3, 0.2)
  expect_equal_rn_generation(
    "rmo_arnold", "test__rmo_arnold_bivariate_R",
    args, n, use_seed)

  ## Low, Low, High
  args[["intensities"]] <- c(0.1, 0.005, 2)
  expect_equal_rn_generation(
    "rmo_arnold", "test__rmo_arnold_bivariate_R",
    args, n, use_seed)

  ## comonotone
  args[["intensities"]] <- c(0, 0, 1)
  expect_equal_rn_generation(
    "rmo_arnold", "test__rmo_arnold_bivariate_R",
    args, n, use_seed)

  ## independence
  args[["intensities"]] <- c(1, 1, 0)
  expect_equal_rn_generation(
    "rmo_arnold", "test__rmo_arnold_bivariate_R",
    args, n, use_seed)
})



## Test that the C++ implementation of the Arnold
## model delivers the same result as the `R` implementation.
test_that("Arnold model implementation in C++", {
  # all equal
  d <- 7L
  intensities <- rep(0.5, times=2^d-1)
  args <- list("d" = d, "intensities" = intensities)
  expect_equal_rn_generation(
    "rmo_arnold", "test__rmo_arnold_R",
    args, n, use_seed)

  # d equals 4 and exchangeable
  d <- 4L
  intensities <- sapply(1:(2^d-1), function(x) {
    cardinality <- sum(sapply(1:d, function(y) {
      Rcpp__is_within(y, x)
    }))

    1 / cardinality
  })
  args <-list("d" = 4L, "intensities" = intensities)
  expect_equal_rn_generation(
    "rmo_arnold", "test__rmo_arnold_R",
    args, n, use_seed)

  ## TODO: Implement tests based on the parametrisation with Bernstein
  ## functions to get realistic test-cases.
})
