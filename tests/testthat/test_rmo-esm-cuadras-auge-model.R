use_seed <- 1623L
n <- 1e2L

## #### Test implementation for the bivariate case ####

test_that("Cuadras-Augé implementation works as intended for d=2", {
  mockery::stub(rmo_esm_cuadras_auge, "Rcpp__rmo_esm_cuadras_auge", rtest__rmo_esm_cuadras_auge)

  ## both equal
  args <- list(
    "d"=2L,
    "alpha"=1,
    "beta"=1
  )
  expect_equal_rn_generation("rmo_esm_cuadras_auge", "test__rmo_esm_cuadras_auge_bivariate",
    args, n, use_seed)

  ## independence
  args[c("alpha", "beta")] <- c(1, 0)
  expect_equal_rn_generation("rmo_esm_cuadras_auge", "test__rmo_esm_cuadras_auge_bivariate",
    args, n, use_seed)

  ## comonotone
  args[c("alpha", "beta")] <- c(0, 1)
  expect_equal_rn_generation("rmo_esm_cuadras_auge", "test__rmo_esm_cuadras_auge_bivariate",
    args, n, use_seed)

  ## alpha = 0.5, beta = 0.3
  args[c("alpha", "beta")] <- c(0.5, 0.3)
  expect_equal_rn_generation("rmo_esm_cuadras_auge", "test__rmo_esm_cuadras_auge_bivariate",
    args, n, use_seed)
})



## #### Test implementation against original `R` version ####

test_that("Cuadras-Augé ESM implementation in Rcpp", {
  mockery::stub(rmo_esm_cuadras_auge, "Rcpp__rmo_esm_cuadras_auge", rtest__rmo_esm_cuadras_auge)

  d <- 7L

  args <- list(
    "d"=d,
    "alpha"=1,
    "beta"=1
  )
  expect_equal_rn_generation("rmo_esm_cuadras_auge", "test__rmo_esm_cuadras_auge",
    args, n, use_seed)

  ## independence
  args[c("alpha", "beta")] <- c(1, 0)
  expect_equal_rn_generation("rmo_esm_cuadras_auge", "test__rmo_esm_cuadras_auge",
    args, n, use_seed)

  ## comonotone
  args[c("alpha", "beta")] <- c(0, 1)
  expect_equal_rn_generation("rmo_esm_cuadras_auge", "test__rmo_esm_cuadras_auge",
    args, n, use_seed)

  ## alpha = 0.5, beta = 0.3
  args[c("alpha", "beta")] <- c(0.5, 0.3)
  expect_equal_rn_generation("rmo_esm_cuadras_auge", "test__rmo_esm_cuadras_auge",
    args, n, use_seed)
})

## TODO: add KS unit test
