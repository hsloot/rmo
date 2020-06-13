context("Exogenous shock model")
use_seed <- 1632L
n <- 100L

# #### Test implementation for the bivariate case ####
#
# Test that the implementation of the exogenous shock model works as expected
# for the bivariate case and different choices for the intensity vector.
test_that("ESM implementation for d = 2", {
  d <- 2L

  ## all equal
  args <- list("d" = d, "intensities" = c(1, 1, 1))
  expect_equal_sampling_result("rmo_esm", "test__rmo_esm_bivariate_R",
    args, n, use_seed)

  ## independence
  args[["intensities"]] <- c(1, 1, 0)
  expect_equal_sampling_result("rmo_esm", "test__rmo_esm_bivariate_R",
    args, n, use_seed)

  ## comonotone
  args[["intensities"]] <- c(0, 0, 1)
  expect_equal_sampling_result("rmo_esm", "test__rmo_esm_bivariate_R",
    args, n, use_seed)

  ## exchangeable, low, high
  args[["intensities"]] <- c(0.1, 0.1, 2)
  expect_equal_sampling_result("rmo_esm", "test__rmo_esm_bivariate_R",
    args, n, use_seed)

  ## exchangeable, high, low
  args[["intensities"]] <- c(3, 3, 0.5)
  expect_equal_sampling_result("rmo_esm", "test__rmo_esm_bivariate_R",
    args, n, use_seed)

  ## Low, High, Low
  args[["intensities"]] <- c(0.5, 3, 0.2)
  expect_equal_sampling_result("rmo_esm", "test__rmo_esm_bivariate_R",
    args, n, use_seed)

  ## Low, Low, High
  args[["intensities"]] <- c(0.1, 0.005, 2)
  expect_equal_sampling_result("rmo_esm", "test__rmo_esm_bivariate_R",
    args, n, use_seed)
})


# #### Test implementation against original `R` version ####
#
# Test that the Rcpp implementation of the exogenous shock model delivers the
# same result as the `R` implementation for various dimensions and choices of
# parameters.
test_that("ESM implementation in C++", {
  # all equal
  d1 <- 3L
  d2 <- 4L
  d <- d1+d2
  intensities <- rep(0.5, times=2^d-1)
  args <- list("d" = d, "intensities" = intensities)
  expect_equal_sampling_result("rmo_esm", "test__rmo_esm_R",
    args, n, use_seed)

  # independence
  args[["intensities"]] <- intensities_linear(d, scale=0.3)
  expect_equal_sampling_result("rmo_esm", "test__rmo_esm_R",
    args, n, use_seed)

  # comonotone
  args[["intensities"]] <- intensities_constant(d, constant=0.7)
  expect_equal_sampling_result("rmo_esm", "test__rmo_esm_R",
    args, n, use_seed)

  # Poisson
  args[["intensities"]] <- intensities_poisson(d, lambda=0.2, eta=0.3)
  expect_equal_sampling_result("rmo_esm", "test__rmo_esm_R",
    args, n, use_seed)

  # Alpha-Stable
  args[["intensities"]] <- intensities_alpha_stable(d, alpha=0.25)
  expect_equal_sampling_result("rmo_esm", "test__rmo_esm_R",
    args, n, use_seed)

  # Gamma
  args[["intensities"]] <- intensities_alpha_stable(d, a=0.4)
  expect_equal_sampling_result("rmo_esm", "test__rmo_esm_R",
    args, n, use_seed)

  # Hierarchical
  args[["intensities"]] <- intensities_hierarchical(d1, d2,
    lambda=0.1, eta=0.3, a=0.2, alpha=0.4)
  expect_equal_sampling_result("rmo_esm", "test__rmo_esm_R",
    args, n, use_seed)
})
