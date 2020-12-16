use_seed <- 1632
n <- 100


## #### Test implementation for the bivariate case ####

test_that("Exchangeable Arnold model for d = 2", {
  mockery::stub(rmo_ex_arnold, "Rcpp__rmo_ex_arnold", rtest__rmo_ex_arnold)

  ## all equal
  args <- list(
    "d" = 2,
    ex_intensities = rep(1, times = 2)
  )
  expect_equal_rn_generation(
    rmo_ex_arnold, testutils.rmo::rmo_ex_arnold_bivariate,
    args, n, use_seed)

  ## independence
  args <- list(
    "d" = 2,
    ex_intensities = ex_intensities_linear(2, scale = 0.7)
  )
  expect_equal_rn_generation(
    rmo_ex_arnold, testutils.rmo::rmo_ex_arnold_bivariate,
    args, n, use_seed)

  ## comonotone
  args <- list(
    "d" = 2,
    ex_intensities = ex_intensities_constant(2, constant = 0.7)
  )
  expect_equal_rn_generation(
    rmo_ex_arnold, testutils.rmo::rmo_ex_arnold_bivariate,
    args, n, use_seed)

  ## Poisson
  args <- list(
    "d" = 2,
    ex_intensities = ex_intensities_poisson(
      2, lambda = 0.2, eta = 0.3
    )
  )
  expect_equal_rn_generation(
    rmo_ex_arnold, testutils.rmo::rmo_ex_arnold_bivariate,
    args, n, use_seed)

  ## Alpha-Stable
  args <- list(
    "d" = 2,
    ex_intensities = ex_intensities_alpha_stable(2, alpha = 0.25)
  )
  expect_equal_rn_generation(
    rmo_ex_arnold, testutils.rmo::rmo_ex_arnold_bivariate,
    args, n, use_seed)

  ## Gamma
  args <- list(
    "d" = 2,
    ex_intensities = ex_intensities_gamma(2, a = 0.4)
  )
  expect_equal_rn_generation(
    rmo_ex_arnold, testutils.rmo::rmo_ex_arnold_bivariate,
    args, n, use_seed)

  ## Pareto
  args <- list(
    "d" = 2,
    "ex_intensities" = ex_intensities_pareto(2, alpha = 0.4, x0 = 1e-4)
  )
  expect_equal_rn_generation(
    rmo_ex_arnold, testutils.rmo::rmo_ex_arnold_bivariate,
    args, n, use_seed)

  ## Inverse-Gaussian
  args <- list(
    "d" = 2,
    "ex_intensities" = ex_intensities_inverse_gaussian(2, eta = 0.5)
  )
  expect_equal_rn_generation(
    rmo_ex_arnold, testutils.rmo::rmo_ex_arnold_bivariate,
    args, n, use_seed)
})



## #### Test alternative implementation in `R` ####

test_that("Alternative implementation in R for d>2", {
  mockery::stub(rmo_ex_arnold, "Rcpp__rmo_ex_arnold", rtest__rmo_ex_arnold)

  d <- 5

  ## all equal
  args <- list(
    "d" = d,
    "ex_intensities" = rep(1, times = d)
  )
  expect_equal_rn_generation(
    rmo_ex_arnold, testutils.rmo::rmo_ex_arnold_naive_recursive,
    args, n, use_seed)

  ## independence
  args <- list(
    "d" = d,
    "ex_intensities" = ex_intensities_linear(d, scale = 0.7)
  )
  expect_equal_rn_generation(
    rmo_ex_arnold, testutils.rmo::rmo_ex_arnold_naive_recursive,
    args, n, use_seed)

  ## comonotone
  args <- list(
    "d" = d,
    "ex_intensities" = ex_intensities_constant(d, constant = 0.7)
  )
  expect_equal_rn_generation(
    rmo_ex_arnold, testutils.rmo::rmo_ex_arnold_naive_recursive,
    args, n, use_seed)

  ## Poisson
  args <- list(
    "d" = d,
    "ex_intensities" = ex_intensities_poisson(
      d, lambda = 0.2, eta = 0.3
    )
  )
  expect_equal_rn_generation(
    rmo_ex_arnold, testutils.rmo::rmo_ex_arnold_naive_recursive,
    args, n, use_seed)

  ## Alpha-stable
  args <- list(
    "d" = d,
    "ex_intensities" = ex_intensities_alpha_stable(d, alpha = 0.25)
  )
  expect_equal_rn_generation(
    rmo_ex_arnold, testutils.rmo::rmo_ex_arnold_naive_recursive,
    args, n, use_seed)

  ## Gamma
  args <- list(
    "d" = d,
    "ex_intensities" = ex_intensities_gamma(d, a = 0.4)
  )
  expect_equal_rn_generation(
    rmo_ex_arnold, testutils.rmo::rmo_ex_arnold_naive_recursive,
    args, n, use_seed)

  ## Pareto
  args <- list(
    "d" = d,
    "ex_intensities" = ex_intensities_pareto(d, alpha = 0.4, x0 = 1e-4)
  )
  expect_equal_rn_generation(
    rmo_ex_arnold, testutils.rmo::rmo_ex_arnold_naive_recursive,
    args, n, use_seed)

  ## Inverse-Gaussian
  args <- list(
    "d" = d,
    "ex_intensities" = ex_intensities_inverse_gaussian(d, eta = 0.5)
  )
  expect_equal_rn_generation(
    rmo_ex_arnold, testutils.rmo::rmo_ex_arnold_naive_recursive,
    args, n, use_seed)
})


## #### Test implementation against original `R` version ####
#
# Test that the Rcpp implementation of the Arnold model delivers the same result
# as the original `R` implementation for various dimensions and choices of
# parameters.
test_that("Exchangeable Arnold model implementation in C++", {
  mockery::stub(rmo_ex_arnold, "Rcpp__rmo_ex_arnold", rtest__rmo_ex_arnold)

  d <- 7

  ## all equal
  args <- list(
    "d" = d,
    "ex_intensities" = rep(1, times = d)
  )
  expect_equal_rn_generation(
    rmo_ex_arnold, testutils.rmo::rmo_ex_arnold_naive,
    args, n, use_seed)

  ## independence
  args <- list(
    "d" = d,
    "ex_intensities" = ex_intensities_linear(d, scale = 0.7)
  )
  expect_equal_rn_generation(
    rmo_ex_arnold, testutils.rmo::rmo_ex_arnold_naive,
    args, n, use_seed)

  ## comonotone
  args <- list(
    "d" = d,
    "ex_intensities" = ex_intensities_constant(d, constant = 0.7)
  )
  expect_equal_rn_generation(
    rmo_ex_arnold, testutils.rmo::rmo_ex_arnold_naive,
    args, n, use_seed)

  ## Poisson
  args <- list(
    "d" = d,
    "ex_intensities" = ex_intensities_poisson(
      d, lambda = 0.2, eta = 0.3
    )
  )
  expect_equal_rn_generation(
    rmo_ex_arnold, testutils.rmo::rmo_ex_arnold_naive,
    args, n, use_seed)

  ## Alpha-stable
  args <- list(
    "d" = d,
    "ex_intensities" = ex_intensities_alpha_stable(d, alpha = 0.25)
  )
  expect_equal_rn_generation(
    rmo_ex_arnold, testutils.rmo::rmo_ex_arnold_naive,
    args, n, use_seed)

  ## Gamma
  args <- list(
    "d" = d,
    "ex_intensities" = ex_intensities_gamma(d, a = 0.4)
  )
  expect_equal_rn_generation(
    rmo_ex_arnold, testutils.rmo::rmo_ex_arnold_naive,
    args, n, use_seed)

  ## Pareto
  args <- list(
    "d" = d,
    "ex_intensities" = ex_intensities_pareto(d, alpha = 0.4, x0 = 1e-4)
  )
  expect_equal_rn_generation(
    rmo_ex_arnold, testutils.rmo::rmo_ex_arnold_naive,
    args, n, use_seed)

  ## Inverse-Gaussian
  args <- list(
    "d" = d,
    "ex_intensities" = ex_intensities_inverse_gaussian(d, eta = 0.5)
  )
  expect_equal_rn_generation(
    rmo_ex_arnold, testutils.rmo::rmo_ex_arnold_naive,
    args, n, use_seed)
})
