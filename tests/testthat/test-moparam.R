d <- 10L

test_that("Exchangeable shock-size arrival intensities are calculated correctly", { # nolint
  bf <- AlphaStableBernsteinFunction(alpha = 0.1033993)

  expect_equal(
    calcExShockSizeArrivalIntensities(bf, d),
    sapply(1:d, function(i) calcIterativeDifference(bf, d - i, i, d, i))
  )

  lambda <- calcExShockArrivalIntensities(bf, d)
  expect_equal(
    calcExShockSizeArrivalIntensities(bf, d),
    sapply(
      1:d,
      function(i) {
        multiply_binomial_coefficient(lambda[[i]], d, i)
      }
    )
  )
})

test_that("calcShockArrivalIntensities parameter is calculated correctly", {
  bf <- AlphaStableBernsteinFunction(alpha = 0.07608632)

  tmp <- sapply(1:d, function(i) calcIterativeDifference(bf, d - i, i))
  lambda <- numeric(2^d - 1)
  for (j in seq_along(lambda)) {
    count <- 0
    for (i in 1:d) {
      count <- count + Rcpp__is_within(i, j)
    }
    lambda[j] <- tmp[count]
  }
  expect_equal(calcShockArrivalIntensities(bf, d), lambda)
})

test_that("ex_qmatrix parameter is calculated correctly", {
  bf <- AlphaStableBernsteinFunction(alpha = 0.6101982)

  ex_qmatrix <- matrix(0, nrow = d + 1, ncol = d + 1)
  ex_qmatrix[1, -1] <- calcExShockSizeArrivalIntensities(bf, d)
  for (i in 1:d) {
    if (i < d) {
      for (j in (i + 1):d) {
        ex_qmatrix[1 + i, 1 + j] <-
          (d - j + 1) / (d - i + 1) * ex_qmatrix[i, j] +
          (j + 1 - i) / (d - i + 1) * ex_qmatrix[i, j + 1]
      }
    }
  }
  diag(ex_qmatrix) <- -apply(ex_qmatrix, 1, sum)
  expect_equal(calcMDCMGeneratorMatrix(bf, d), ex_qmatrix)

  expect_equal(
    calcMDCMGeneratorMatrix(bf, d), calcMDCMGeneratorMatrix(bf, d + 1)[-1, -1]
  )
})

test_that("Sum of shock-size arrival intensities is calculated correctly (base case)", { # nolint
  bf <- AlphaStableBernsteinFunction(alpha = 0.8598596)

  expect_equal(
    sum(calcExShockSizeArrivalIntensities(bf, d)),
    calcValue(bf, d)
  )
})

test_that("Sum of shock-size arrival intensities is calculated correctly (corner case)", { # nolint
  bf <- AlphaStableBernsteinFunction(log2(2 - 99.999e-2))
  d <- 125

  expect_equal(
    sum(calcExShockSizeArrivalIntensities(bf, d)),
    calcValue(bf, d)
  )
})
