context("Cuadras-Auge exogenous shock model")
use_seed <- 1623L

test_that("Cuadras-Augé implementation works as intended", {
  n <- 25L

  ## alpha = 0.5, beta = 0.3
  args <- list("d"=2L, "alpha"=0.5, "beta"=0.3)
  expect_equal_sampling_result("rmo_esm_cuadras_auge", "test__rmo_esm_cuadras_auge_bivariate_R",
    args, n, use_seed)

  ## alpha = 0, beta = 1
  args[c("alpha", "beta")] <- c(0, 1)
  expect_equal_sampling_result("rmo_esm_cuadras_auge", "test__rmo_esm_cuadras_auge_bivariate_R",
    args, n, use_seed)

  ## alpha = 1, beta = 0
  args[c("alpha", "beta")] <- c(1, 0)
  expect_equal_sampling_result("rmo_esm_cuadras_auge", "test__rmo_esm_cuadras_auge_bivariate_R",
    args, n, use_seed)
})



## Test that the C++ implementation of the exogenous shock
## model delivers the same result as the `R` implementation.
test_that("ESM implementation in C++", {
  n <- 25L

  ## alpha = 0.5, beta = 0.3
  args <- list("d"=2L, "alpha"=0.5, "beta"=0.3)
  expect_equal_sampling_result("rmo_esm_cuadras_auge", "test__rmo_esm_cuadras_auge_R",
                               args, n, use_seed)

  ## d = 5L, alpha = 0.5, beta = 0.3
  args["d"] <- 5L
  expect_equal_sampling_result("rmo_esm_cuadras_auge", "test__rmo_esm_cuadras_auge_R",
                              args, n, use_seed)
})
