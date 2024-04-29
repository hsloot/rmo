bf_poisson <- PoissonBernsteinFunction(eta = 0.1336104)

test_that("Initialize `PoissonBernsteinFunction`", {
  expect_s4_class(bf_poisson, "PoissonBernsteinFunction")

  expect_error(PoissonBernsteinFunction(eta = -0.2))
  expect_error(PoissonBernsteinFunction(eta = c(0.1, 0.2)))
  expect_error(PoissonBernsteinFunction(eta = Inf))
  expect_error(PoissonBernsteinFunction(eta = NA))
  expect_error(PoissonBernsteinFunction(eta = NaN))
})

cscale <- sqrt(2)
x <- seq(0, 10, by = 0.25)
actual_fn <- function(x, eta) {
  1 - exp(-eta * x)
}

test_that("`calcIterativeDifference` calculates expected values", {
  expect_equal(
    calcIterativeDifference(bf_poisson, x),
    actual_fn(x, bf_poisson@eta)
  )

  expect_equal(
    calcIterativeDifference(bf_poisson, x),
    calcValue(bf_poisson, x)
  )

  expect_equal(
    calcIterativeDifference(bf_poisson, x, cscale = cscale),
    actual_fn(cscale * x, bf_poisson@eta)
  )

  expect_equal(
    calcIterativeDifference(bf_poisson, x, cscale = cscale),
    calcIterativeDifference(
      bf_poisson, x,
      cscale = cscale,
      method = "levy",
      tolerance = testthat_tolerance()
    )
  )
})

d <- 7

test_that("`calcExShockSizeArrivalIntensities` calculates expected values", {
  expect_equal(
    calcExShockSizeArrivalIntensities(bf_poisson, d),
    calc_ex_shock_size_arrival_intensities_naive(
      actual_fn, d, eta = bf_poisson@eta
    )
  )

  expect_equal(
    calcExShockSizeArrivalIntensities(bf_poisson, d, cscale = cscale),
    calc_ex_shock_size_arrival_intensities_naive(
      actual_fn, d,
      eta = bf_poisson@eta,
      cscale = cscale
    )
  )

  expect_equal(
    calcExShockSizeArrivalIntensities(bf_poisson, d, cscale = cscale),
    calcExShockSizeArrivalIntensities(
      bf_poisson, d,
      cscale = cscale,
      method = "levy",
      tolerance = testthat_tolerance()
    )
  )
})

test_that("`calcMDCMGeneratorMatrix` calculates expected values", {
  expect_equal(
    calcMDCMGeneratorMatrix(bf_poisson, d),
    mdcm_generator_matrix(
      actual_fn, d,
      eta = bf_poisson@eta
    )
  )

  expect_equal(
    calcMDCMGeneratorMatrix(bf_poisson, d, cscale = cscale),
    mdcm_generator_matrix(
      actual_fn, d,
      cscale = cscale,
      eta = bf_poisson@eta
    )
  )

  expect_equal(
    calcMDCMGeneratorMatrix(bf_poisson, d, cscale = cscale),
    calcMDCMGeneratorMatrix(
      bf_poisson, d,
      cscale = cscale,
      method = "levy", tolerance = testthat_tolerance()
    )
  )
})
