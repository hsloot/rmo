context("Exogenous shock model")

## Test that the implementation exogenous shock model
## works as expected for d = 2.
test_that("ESM implementation for d = 2", {
  n <- 100
  intensities <- c(0.5, 0.4, 0.2)

  set.seed(1632)
  x <- rmo_esm(n, 2, intensities)

  set.seed(1632)
  y <- test__rmo_esm_bivariate_R(n, intensities)

  expect_equal(x, y)
})
