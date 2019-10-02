context("Exogenous shock model")

test_that("ESM implementation", {
  n <- 10
  d <- 2
  intensities <- c(0.5, 0.4, 0.2)

  set.seed(1632)
  x <- rmo_esm(n, d, intensities)

  set.seed(1632)
  y <- matrix(0, nrow = n, ncol = d)
  for (i in 1:n) {
    y[i, ] <- pmin(1/intensities[1:2] * rexp(2), c(1, 1)/intensities[3] * rexp(1))
  }

  expect_equal(x, y)
})
