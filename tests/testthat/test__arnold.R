context("Arnold model")

## Test that the implementation Arnold model
## works as expected for d = 2.
test_that("Arnold model implementation for d = 2", {
  n <- 100
  intensities <- c(0.5, 0.4, 0.2)

  set.seed(1632)
  x <- rmo_arnold(n, 2, intensities)

  set.seed(1632)
  y <- test__rmo_arnold_bivariate_R(n, intensities)

  expect_equal(x, y)
})
