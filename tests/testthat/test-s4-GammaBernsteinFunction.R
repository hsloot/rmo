bf_gamma <- GammaBernsteinFunction(a = 0.1253985)

test_that("Initialize `GammaBernsteinFunction`", {
  expect_s4_class(bf_gamma, class = "GammaBernsteinFunction")

  expect_error(GammaBernsteinFunction(a = 0))
  expect_error(GammaBernsteinFunction(a = c(1, 2)))
  expect_error(GammaBernsteinFunction(a = -1))
  expect_error(GammaBernsteinFunction(a = Inf))
  expect_error(GammaBernsteinFunction(a = NA))
  expect_error(GammaBernsteinFunction(a = NaN))
})

cscale <- sqrt(2)
x <- seq(0, 10, by = 0.25)
actual_fn <- function(x, a) {
  log(1 + x / a)
}

test_that("`calcIterativeDifference` calculates expected values", {
  expect_equal(
    calcIterativeDifference(bf_gamma, x),
    actual_fn(x, bf_gamma@a)
  )

  expect_equal(
    calcIterativeDifference(bf_gamma, x),
    calcValue(bf_gamma, x)
  )

  expect_equal(
    calcIterativeDifference(bf_gamma, x, cscale = cscale),
    actual_fn(cscale * x, bf_gamma@a)
  )
})

d <- 7

test_that("`calcExShockSizeArrivalIntensities` calculates expected values", {
  expect_equal(
    calcExShockSizeArrivalIntensities(bf_gamma, d),
    calc_ex_shock_size_arrival_intensities_naive(
      actual_fn, d,
      a = bf_gamma@a
    )
  )

  expect_equal(
    calcExShockSizeArrivalIntensities(bf_gamma, d, cscale = cscale),
    calc_ex_shock_size_arrival_intensities_naive(
      actual_fn, d,
      a = bf_gamma@a,
      cscale = cscale
    )
  )

  expect_equal(
    calcExShockSizeArrivalIntensities(bf_gamma, d, cscale = cscale),
    calcExShockSizeArrivalIntensities(
      bf_gamma, d,
      cscale = cscale,
      method = "levy",
      tolerance = testthat_tolerance()
    )
  )

  expect_equal(
    calcExShockSizeArrivalIntensities(bf_gamma, d, cscale = cscale),
    calcExShockSizeArrivalIntensities(
      bf_gamma, d,
      cscale = cscale,
      method = "stieltjes", tolerance = testthat_tolerance()
    )
  )
})

test_that("`calcMDCMGeneratorMatrix` calculates expected values", {
  expect_equal(
    calcMDCMGeneratorMatrix(bf_gamma, d),
    mdcm_generator_matrix_naive(
      actual_fn, d,
      a = bf_gamma@a
    )
  )

  expect_equal(
    calcMDCMGeneratorMatrix(bf_gamma, d, cscale = cscale),
    mdcm_generator_matrix_naive(
      actual_fn, d,
      cscale = cscale,
      a = bf_gamma@a
    )
  )

  expect_equal(
    calcMDCMGeneratorMatrix(bf_gamma, d, cscale = cscale),
    calcMDCMGeneratorMatrix(
      bf_gamma, d,
      cscale = cscale,
      method = "levy", tolerance = testthat_tolerance()
    )
  )

  expect_equal(
    calcMDCMGeneratorMatrix(bf_gamma, d, cscale = cscale),
    calcMDCMGeneratorMatrix(
      bf_gamma, d,
      cscale = cscale,
      method = "stieltjes", tolerance = testthat_tolerance()
    )
  )
})
