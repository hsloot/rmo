bf_scaled_bf <- ScaledBernsteinFunction(
  scale = 4.638729,
  original = AlphaStableBernsteinFunction(alpha = 0.2188059)
)

test_that("Initialize `ScaledBernsteinFunction`", {
  expect_s4_class(bf_scaled_bf, "ScaledBernsteinFunction")

  expect_error(
    ScaledBernsteinFunction(
      scale = -1,
      ConstantBernsteinFunction(constant = 1)
    )
  )
  expect_error(
    ScaledBernsteinFunction(
      scale = Inf,
      ConstantBernsteinFunction(constant = 1)
    )
  )
  expect_error(
    ScaledBernsteinFunction(
      scale = NA,
      ConstantBernsteinFunction(constant = 1)
    )
  )
  expect_error(
    ScaledBernsteinFunction(
      scale = NaN,
      ConstantBernsteinFunction(constant = 1)
    )
  )
})

cscale <- sqrt(2)
x <- seq(0, 10, by = 0.25)
actual_fn <- function(x, scale, alpha) {
  scale * x^alpha
}

test_that("`calcIterativeDifference` calculates expected values", {
  expect_equal(
    calcIterativeDifference(bf_scaled_bf, x),
    actual_fn(
      x,
      scale = bf_scaled_bf@scale, alpha = bf_scaled_bf@original@alpha
    )
  )

  expect_equal(
    calcIterativeDifference(bf_scaled_bf, x),
    calcValue(bf_scaled_bf, x)
  )

  expect_equal(
    calcIterativeDifference(bf_scaled_bf, x, cscale = cscale),
    actual_fn(
      cscale * x,
      scale = bf_scaled_bf@scale,
      alpha = bf_scaled_bf@original@alpha
    )
  )
})

d <- 7

test_that("`calcExShockSizeArrivalIntensities` calculates expected values", {
  expect_equal(
    calcExShockSizeArrivalIntensities(bf_scaled_bf, d),
    calc_ex_shock_size_arrival_intensities_naive(
      actual_fn, d,
      scale = bf_scaled_bf@scale,
      alpha = bf_scaled_bf@original@alpha
    )
  )

  expect_equal(
    calcExShockSizeArrivalIntensities(bf_scaled_bf, d, cscale = cscale),
    calc_ex_shock_size_arrival_intensities_naive(
      actual_fn, d,
      scale = bf_scaled_bf@scale,
      alpha = bf_scaled_bf@original@alpha,
      cscale = cscale
    )
  )

  expect_equal(
    calcExShockSizeArrivalIntensities(bf_scaled_bf, d, cscale = cscale),
    calcExShockSizeArrivalIntensities(
      bf_scaled_bf, d,
      cscale = cscale,
      method = "levy",
      tolerance = testthat_tolerance()
    )
  )

  expect_equal(
    calcExShockSizeArrivalIntensities(bf_scaled_bf, d, cscale = cscale),
    calcExShockSizeArrivalIntensities(
      bf_scaled_bf, d,
      cscale = cscale,
      method = "stieltjes", tolerance = testthat_tolerance()
    )
  )
})

test_that("`calcMDCMGeneratorMatrix` calculates expected values", {
  expect_equal(
    calcMDCMGeneratorMatrix(bf_scaled_bf, d),
    mdcm_generator_matrix(
      actual_fn, d,
      scale = bf_scaled_bf@scale,
      alpha = bf_scaled_bf@original@alpha
    )
  )

  expect_equal(
    calcMDCMGeneratorMatrix(bf_scaled_bf, d, cscale = cscale),
    mdcm_generator_matrix(
      actual_fn, d,
      cscale = cscale,
      scale = bf_scaled_bf@scale,
      alpha = bf_scaled_bf@original@alpha
    )
  )

  expect_equal(
    calcMDCMGeneratorMatrix(bf_scaled_bf, d, cscale = cscale),
    calcMDCMGeneratorMatrix(
      bf_scaled_bf, d,
      cscale = cscale,
      method = "levy", tolerance = testthat_tolerance()
    )
  )

  expect_equal(
    calcMDCMGeneratorMatrix(bf_scaled_bf, d, cscale = cscale),
    calcMDCMGeneratorMatrix(
      bf_scaled_bf, d,
      cscale = cscale,
      method = "stieltjes", tolerance = testthat_tolerance()
    )
  )
})
