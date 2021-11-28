use_seed <- 1632
n <- 100


## #### Test implementation for the bivariate case ####

test_that("Exchangeable Arnold model for d = 2", {
  mockery::stub(rexmo_mdcm, "Rcpp__rexmo_mdcm", rtest__rexmo_mdcm)

  ## all equal
  args <- list(
    "d" = 2,
    ex_intensities = sapply(1:2, function(i) choose(2, i))
  )
  expect_equal_rn_generation(
    rexmo_mdcm, testutils.rmo::rexmo_mdcm_bivariate,
    args, n, use_seed)

  ## independence
  args <- list(
    "d" = 2,
    ex_intensities = ex_intensities_linear(2, scale = 0.7)
  )
  expect_equal_rn_generation(
    rexmo_mdcm, testutils.rmo::rexmo_mdcm_bivariate,
    args, n, use_seed)

  ## comonotone
  args <- list(
    "d" = 2,
    ex_intensities = ex_intensities_constant(2, constant = 0.7)
  )
  expect_equal_rn_generation(
    rexmo_mdcm, testutils.rmo::rexmo_mdcm_bivariate,
    args, n, use_seed)

  ## Poisson
  args <- list(
    "d" = 2,
    ex_intensities = ex_intensities_poisson(
      2, eta = 0.3
    )
  )
  expect_equal_rn_generation(
    rexmo_mdcm, testutils.rmo::rexmo_mdcm_bivariate,
    args, n, use_seed)

  ## Alpha-Stable
  args <- list(
    "d" = 2,
    ex_intensities = ex_intensities_alpha_stable(2, alpha = 0.25)
  )
  expect_equal_rn_generation(
    rexmo_mdcm, testutils.rmo::rexmo_mdcm_bivariate,
    args, n, use_seed)

  ## Gamma
  args <- list(
    "d" = 2,
    ex_intensities = ex_intensities_gamma(2, a = 0.4)
  )
  expect_equal_rn_generation(
    rexmo_mdcm, testutils.rmo::rexmo_mdcm_bivariate,
    args, n, use_seed)

  ## Pareto
  args <- list(
    "d" = 2,
    "ex_intensities" = ex_intensities_pareto(2, alpha = 0.4, x0 = 1e-4)
  )
  expect_equal_rn_generation(
    rexmo_mdcm, testutils.rmo::rexmo_mdcm_bivariate,
    args, n, use_seed)

  ## Inverse-Gaussian
  args <- list(
    "d" = 2,
    "ex_intensities" = ex_intensities_inverse_gaussian(2, eta = 0.5)
  )
  expect_equal_rn_generation(
    rexmo_mdcm, testutils.rmo::rexmo_mdcm_bivariate,
    args, n, use_seed)
})



## #### Test alternative implementation in `R` ####

test_that("Alternative implementation in R for d>2", {
  mockery::stub(rexmo_mdcm, "Rcpp__rexmo_mdcm", rtest__rexmo_mdcm)

  d <- 5

  ## all equal
  args <- list(
    "d" = d,
    "ex_intensities" = sapply(1:d, function(i) choose(d, i))
  )
  expect_equal_rn_generation(
    rexmo_mdcm, testutils.rmo::rexmo_mdcm_naive_recursive,
    args, n, use_seed)

  ## independence
  args <- list(
    "d" = d,
    "ex_intensities" = ex_intensities_linear(d, scale = 0.7)
  )
  expect_equal_rn_generation(
    rexmo_mdcm, testutils.rmo::rexmo_mdcm_naive_recursive,
    args, n, use_seed)

  ## comonotone
  args <- list(
    "d" = d,
    "ex_intensities" = ex_intensities_constant(d, constant = 0.7)
  )
  expect_equal_rn_generation(
    rexmo_mdcm, testutils.rmo::rexmo_mdcm_naive_recursive,
    args, n, use_seed)

  ## Poisson
  args <- list(
    "d" = d,
    "ex_intensities" = ex_intensities_poisson(
      d, eta = 0.3
    )
  )
  expect_equal_rn_generation(
    rexmo_mdcm, testutils.rmo::rexmo_mdcm_naive_recursive,
    args, n, use_seed)

  ## Alpha-stable
  args <- list(
    "d" = d,
    "ex_intensities" = ex_intensities_alpha_stable(d, alpha = 0.25)
  )
  expect_equal_rn_generation(
    rexmo_mdcm, testutils.rmo::rexmo_mdcm_naive_recursive,
    args, n, use_seed)

  ## Gamma
  args <- list(
    "d" = d,
    "ex_intensities" = ex_intensities_gamma(d, a = 0.4)
  )
  expect_equal_rn_generation(
    rexmo_mdcm, testutils.rmo::rexmo_mdcm_naive_recursive,
    args, n, use_seed)

  ## Pareto
  args <- list(
    "d" = d,
    "ex_intensities" = ex_intensities_pareto(d, alpha = 0.4, x0 = 1e-4)
  )
  expect_equal_rn_generation(
    rexmo_mdcm, testutils.rmo::rexmo_mdcm_naive_recursive,
    args, n, use_seed)

  ## Inverse-Gaussian
  args <- list(
    "d" = d,
    "ex_intensities" = ex_intensities_inverse_gaussian(d, eta = 0.5)
  )
  expect_equal_rn_generation(
    rexmo_mdcm, testutils.rmo::rexmo_mdcm_naive_recursive,
    args, n, use_seed)
})


## #### Test implementation against original `R` version ####
#
# Test that the Rcpp implementation of the Arnold model delivers the same result
# as the original `R` implementation for various dimensions and choices of
# parameters.
test_that("Exchangeable Arnold model implementation in C++", {
  mockery::stub(rexmo_mdcm, "Rcpp__rexmo_mdcm", rtest__rexmo_mdcm)

  d <- 7

  ## all equal
  args <- list(
    "d" = d,
    "ex_intensities" = sapply(1:d, function(i) choose(d, i))
  )
  expect_equal_rn_generation(
    rexmo_mdcm, testutils.rmo::rexmo_mdcm_naive,
    args, n, use_seed)

  ## independence
  args <- list(
    "d" = d,
    "ex_intensities" = ex_intensities_linear(d, scale = 0.7)
  )
  expect_equal_rn_generation(
    rexmo_mdcm, testutils.rmo::rexmo_mdcm_naive,
    args, n, use_seed)

  ## comonotone
  args <- list(
    "d" = d,
    "ex_intensities" = ex_intensities_constant(d, constant = 0.7)
  )
  expect_equal_rn_generation(
    rexmo_mdcm, testutils.rmo::rexmo_mdcm_naive,
    args, n, use_seed)

  ## Poisson
  args <- list(
    "d" = d,
    "ex_intensities" = ex_intensities_poisson(
      d, eta = 0.3
    )
  )
  expect_equal_rn_generation(
    rexmo_mdcm, testutils.rmo::rexmo_mdcm_naive,
    args, n, use_seed)

  ## Alpha-stable
  args <- list(
    "d" = d,
    "ex_intensities" = ex_intensities_alpha_stable(d, alpha = 0.25)
  )
  expect_equal_rn_generation(
    rexmo_mdcm, testutils.rmo::rexmo_mdcm_naive,
    args, n, use_seed)

  ## Gamma
  args <- list(
    "d" = d,
    "ex_intensities" = ex_intensities_gamma(d, a = 0.4)
  )
  expect_equal_rn_generation(
    rexmo_mdcm, testutils.rmo::rexmo_mdcm_naive,
    args, n, use_seed)

  ## Pareto
  args <- list(
    "d" = d,
    "ex_intensities" = ex_intensities_pareto(d, alpha = 0.4, x0 = 1e-4)
  )
  expect_equal_rn_generation(
    rexmo_mdcm, testutils.rmo::rexmo_mdcm_naive,
    args, n, use_seed)

  ## Inverse-Gaussian
  args <- list(
    "d" = d,
    "ex_intensities" = ex_intensities_inverse_gaussian(d, eta = 0.5)
  )
  expect_equal_rn_generation(
    rexmo_mdcm, testutils.rmo::rexmo_mdcm_naive,
    args, n, use_seed)
})
