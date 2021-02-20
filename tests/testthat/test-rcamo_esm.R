use_seed <- 1623
n <- 1e2

## #### Test implementation for the bivariate case ####

test_that("Cuadras-Augé implementation works as intended for d=2", {
  mockery::stub(rcamo_esm, "Rcpp__rcamo_esm", rtest__rcamo_esm)

  ## both equal
  args <- list("d" = 2, "alpha" = 1, "beta" = 1)
  expect_equal_rn_generation(rcamo_esm, testutils.rmo::rcamo_esm_bivariate,
    args, n, use_seed)

  ## independence
  args <- list("d" = 2, "alpha" = 1, "beta" = 0)
  expect_equal_rn_generation(rcamo_esm, testutils.rmo::rcamo_esm_bivariate,
    args, n, use_seed)

  ## comonotone
  args <- list("d" = 2, "alpha" = 0, "beta" = 1)
  expect_equal_rn_generation(rcamo_esm, testutils.rmo::rcamo_esm_bivariate,
    args, n, use_seed)

  ## alpha = 0.5, beta = 0.3
  args <- list("d" = 2, "alpha" = 0.5, "beta" = 0.3)
  expect_equal_rn_generation(rcamo_esm, testutils.rmo::rcamo_esm_bivariate,
    args, n, use_seed)
})



## #### Test implementation against original `R` version ####

test_that("Cuadras-Augé ESM implementation in Rcpp", {
  mockery::stub(rcamo_esm, "Rcpp__rcamo_esm", rtest__rcamo_esm)

  d <- 7

  args <- list("d" = d, "alpha" = 1, "beta" = 1)
  expect_equal_rn_generation(rcamo_esm, testutils.rmo::rcamo_esm_naive,
    args, n, use_seed)

  ## independence
  args <- list("d" = d, "alpha" = 1, "beta" = 0)
  expect_equal_rn_generation(rcamo_esm, testutils.rmo::rcamo_esm_naive,
    args, n, use_seed)

  ## comonotone
  args <- list("d" = d, "alpha" = 0, "beta" = 1)
  expect_equal_rn_generation(rcamo_esm, testutils.rmo::rcamo_esm_naive,
    args, n, use_seed)

  ## alpha = 0.5, beta = 0.3
  args <- list("d" = d, "alpha" = 0.5, "beta" = 0.3)
  expect_equal_rn_generation(rcamo_esm, testutils.rmo::rcamo_esm_naive,
    args, n, use_seed)
})
