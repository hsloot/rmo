context("Exogenous shock model")

## Test that the implementation exogenous shock model
## works as expected for d = 2.
test_that("ESM implementation for d = 2", {
  n <- 100
  intensities <- c(0.5, 0.4, 0.2)

  set.seed(1632)
  x <- rmo_esm(n, 2, intensities)

  set.seed(1632)
  y <- matrix(0, nrow = n, ncol = 2)
  for (i in 1:n) {
    y[i, ] <- pmin(1/intensities[1:2] * rexp(2), c(1, 1)/intensities[3] * rexp(1))
  }

  expect_equal(x, y)
})
