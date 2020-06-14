context("Exchangeable Arnold model")
use_seed <- 1632L
n <- 100L


## #### Test implementation for the bivariate case ####

test_that("Exchangeable Arnold model for d = 2", {
  ## all equal
  args <- list(
    "d" = 2L,
    ex_intensities = c(1, 1)
  )
  expect_equal_rn_generation(
    "rmo_ex_arnold", "test__rmo_ex_arnold_bivariate",
    args, n, use_seed)

  ## independence
  args[["ex_intensities"]] <- c(1, 0)
  expect_equal_rn_generation(
    "rmo_ex_arnold", "test__rmo_ex_arnold_bivariate",
    args, n, use_seed)

  ## comonotone
  args[["ex_intensities"]] <- c(0, 1)
  expect_equal_rn_generation(
    "rmo_ex_arnold", "test__rmo_ex_arnold_bivariate",
    args, n, use_seed)

  ## low, high
  args[["ex_intensities"]] <- c(0.5, 2)
  expect_equal_rn_generation(
    "rmo_ex_arnold", "test__rmo_ex_arnold_bivariate",
    args, n, use_seed)

  ## high, low
  args[["ex_intensities"]] <- c(3, 0.2)
  expect_equal_rn_generation(
    "rmo_ex_arnold", "test__rmo_ex_arnold_bivariate",
    args, n, use_seed)
})



## #### Test alternative implementation in `R` ####

test_that("Alternative implementation in R for d>2", {
  d <- 5L
  ## all equal
  args <- list(
    "d" = d,
    "ex_intensities" = rep(1, times=d)
  )
  expect_equal_rn_generation(
    "rmo_ex_arnold", "test__rmo_ex_arnold_alternative",
    args, n, use_seed)

  ## independence
  args[["ex_intensities"]] <- ex_intensities_linear(d, scale=0.7)
  expect_equal_rn_generation(
    "rmo_ex_arnold", "test__rmo_ex_arnold_alternative",
    args, n, use_seed)

  ## comonotone
  args[["ex_intensities"]] <- ex_intensities_constant(d, constant=0.7)
  expect_equal_rn_generation(
    "rmo_ex_arnold", "test__rmo_ex_arnold_alternative",
    args, n, use_seed)

  ## Poisson
  args[["ex_intensities"]] <- ex_intensities_poisson(d, lambda=0.2, eta=0.3)
  expect_equal_rn_generation(
    "rmo_ex_arnold", "test__rmo_ex_arnold_alternative",
    args, n, use_seed)

  ## Alpha-stable
  args[["ex_intensities"]] <- ex_intensities_alpha_stable(d, alpha=0.25)
  expect_equal_rn_generation(
    "rmo_ex_arnold", "test__rmo_ex_arnold_alternative",
    args, n, use_seed)

  ## Gamma
  args[["ex_intensities"]] <- ex_intensities_gamma(d, a=0.4)
  expect_equal_rn_generation(
    "rmo_ex_arnold", "test__rmo_ex_arnold_alternative",
    args, n, use_seed)
})


## #### Test implementation against original `R` version ####
#
# Test that the Rcpp implementation of the Arnold model delivers the same result
# as the original `R` implementation for various dimensions and choices of
# parameters.
test_that("Exchangeable Arnold model implementation in C++", {
  d <- 7L
  ## all equal
  args <- list(
    "d" = d,
    "ex_intensities" = rep(1, times=d)
  )
  expect_equal_rn_generation(
    "rmo_ex_arnold", "test__rmo_ex_arnold",
    args, n, use_seed)

  ## independence
  args[["ex_intensities"]] <- ex_intensities_linear(d, scale=0.7)
  expect_equal_rn_generation(
    "rmo_ex_arnold", "test__rmo_ex_arnold",
    args, n, use_seed)

  ## comonotone
  args[["ex_intensities"]] <- ex_intensities_constant(d, constant=0.7)
  expect_equal_rn_generation(
    "rmo_ex_arnold", "test__rmo_ex_arnold",
    args, n, use_seed)

  ## Poisson
  args[["ex_intensities"]] <- ex_intensities_poisson(d, lambda=0.2, eta=0.3)
  expect_equal_rn_generation(
    "rmo_ex_arnold", "test__rmo_ex_arnold",
    args, n, use_seed)

  ## Alpha-stable
  args[["ex_intensities"]] <- ex_intensities_alpha_stable(d, alpha=0.25)
  expect_equal_rn_generation(
    "rmo_ex_arnold", "test__rmo_ex_arnold",
    args, n, use_seed)

  ## Gamma
  args[["ex_intensities"]] <- ex_intensities_gamma(d, a=0.4)
  expect_equal_rn_generation(
    "rmo_ex_arnold", "test__rmo_ex_arnold",
    args, n, use_seed)
})
