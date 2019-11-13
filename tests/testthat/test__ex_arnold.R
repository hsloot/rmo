context("Exchangeable Arnold model")

## Test that the implementation of the modified version of the
## Arnold model for exchangeable distributions works as intended
## for d = 2.
test_that("Exchangeable Arnold model for d = 2", {
  n <- 100
  ex_intensities <- c(0.5, 1)

  set.seed(1632)
  x <- rmo_ex_arnold(n, 2, ex_intensities)

  set.seed(1632)
  y <- test__rmo_ex_arnold_bivariate_R(n, ex_intensities)

  expect_equal(x, y)
})



## Test that alternativ eimplementation based on the `a`
## parameters is equivalent for d = 5.
test_that("Alternative implementation", {
  n <- 100

  ex_intensities <- c(0.4, 0.3, 0.2, 0.2, 0.1)

  set.seed(1632)
  x <- rmo_ex_arnold(n, 5, ex_intensities)

  set.seed(1632)
  y <- test__rmo_ex_arnold_alternative_R(n, 5, ex_intensities)

  expect_equal(x, y)
})
