context("binomial coefficient functions")

test_that("`binomial_coefficient` works as expected", {
  n <- 50
  x <- matrix(0, nrow = n, ncol = n + 1)
  for (i in 1:n) {
    x[i, 1:(i + 1)] <- sapply(0:i, function(k) choose(i, k))
  }
  y <- matrix(0, nrow = n, ncol = n + 1)
  for (i in 1:n) {
    y[i, 1:(i + 1)] <- sapply(0:i, function(k) mo_internal__binomial_coefficient(i, k))
  }
  expect_equal(x, y)
})


test_that("`binomial_coefficient_factor` works as expected", {
  value <- pi * 1e-7
  n <- 75
  x <- matrix(0, nrow = n, ncol = n + 1)
  for (i in 1:n) {
    x[i, 1:(i + 1)] <- sapply(0:i, function(k) value * choose(i, k))
  }
  y <- matrix(0, nrow = n, ncol = n + 1)
  for (i in 1:n) {
    y[i, 1:(i + 1)] <- sapply(
      0:i, function(k) mo_internal__binomial_coefficient_factor(value, i, k))
  }
  expect_equal(x, y)
})
