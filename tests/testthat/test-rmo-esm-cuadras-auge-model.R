use_seed <- 1623
n <- 1e2

## #### Test implementation for the bivariate case ####

test_that("Cuadras-Augé implementation works as intended for d=2", {
  mockery::stub(rmo_esm_cuadras_auge, "Rcpp__rmo_esm_cuadras_auge", rtest__rmo_esm_cuadras_auge)

  ## both equal
  args <- list("d" = 2, "alpha" = 1, "beta" = 1)
  expect_equal_rn_generation(rmo_esm_cuadras_auge, testutils.rmo::rmo_esm_cuadras_auge_bivariate,
    args, n, use_seed)

  ## independence
  args <- list("d" = 2, "alpha" = 1, "beta" = 0)
  expect_equal_rn_generation(rmo_esm_cuadras_auge, testutils.rmo::rmo_esm_cuadras_auge_bivariate,
    args, n, use_seed)

  ## comonotone
  args <- list("d" = 2, "alpha" = 0, "beta" = 1)
  expect_equal_rn_generation(rmo_esm_cuadras_auge, testutils.rmo::rmo_esm_cuadras_auge_bivariate,
    args, n, use_seed)

  ## alpha = 0.5, beta = 0.3
  args <- list("d" = 2, "alpha" = 0.5, "beta" = 0.3)
  expect_equal_rn_generation(rmo_esm_cuadras_auge, testutils.rmo::rmo_esm_cuadras_auge_bivariate,
    args, n, use_seed)
})



## #### Test implementation against original `R` version ####

test_that("Cuadras-Augé ESM implementation in Rcpp", {
  mockery::stub(rmo_esm_cuadras_auge, "Rcpp__rmo_esm_cuadras_auge", rtest__rmo_esm_cuadras_auge)

  d <- 7

  args <- list("d" = d, "alpha" = 1, "beta" = 1)
  expect_equal_rn_generation(rmo_esm_cuadras_auge, testutils.rmo::rmo_esm_cuadras_auge_naive,
    args, n, use_seed)

  ## independence
  args <- list("d" = d, "alpha" = 1, "beta" = 0)
  expect_equal_rn_generation(rmo_esm_cuadras_auge, testutils.rmo::rmo_esm_cuadras_auge_naive,
    args, n, use_seed)

  ## comonotone
  args <- list("d" = d, "alpha" = 0, "beta" = 1)
  expect_equal_rn_generation(rmo_esm_cuadras_auge, testutils.rmo::rmo_esm_cuadras_auge_naive,
    args, n, use_seed)

  ## alpha = 0.5, beta = 0.3
  args <- list("d" = d, "alpha" = 0.5, "beta" = 0.3)
  expect_equal_rn_generation(rmo_esm_cuadras_auge, testutils.rmo::rmo_esm_cuadras_auge_naive,
    args, n, use_seed)
})

## TODO: add KS unit test
