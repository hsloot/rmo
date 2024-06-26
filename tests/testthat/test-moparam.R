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

test_that("Shock arrival intensities are calculated correctly", {
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

test_that("MDCM generator matrix is calculated correctly", {
  bf <- AlphaStableBernsteinFunction(alpha = 0.6101982)

  generator <- matrix(0, nrow = d + 1, ncol = d + 1)
  generator[1, -1] <- calcExShockSizeArrivalIntensities(bf, d)
  for (i in 1:d) {
    if (i < d) {
      for (j in (i + 1):d) {
        generator[1 + i, 1 + j] <-
          (d - j + 1) / (d - i + 1) * generator[i, j] +
          (j + 1 - i) / (d - i + 1) * generator[i, j + 1]
      }
    }
  }
  diag(generator) <- -apply(generator, 1, sum)
  expect_equal(calcMDCMGeneratorMatrix(bf, d), generator)

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
