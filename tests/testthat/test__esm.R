context("Exogenous shock model")
use_seed <- 1632L

## Test that the implementation exogenous shock model
## works as expected for d = 2.
test_that("ESM implementation for d = 2", {
  n <- 100L

  args <- list("d" = 2L, "intensities" = c(0.5, 0.4, 0.2))
  expect_equal_sampling_result("rmo_esm", "test__rmo_esm_bivariate_R",
                               args, n, use_seed)
})
