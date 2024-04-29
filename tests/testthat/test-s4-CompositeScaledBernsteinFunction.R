bf_composite_scaled_bf <- CompositeScaledBernsteinFunction(
  cscale = 0.887174,
  original = AlphaStableBernsteinFunction(alpha = 0.7281229)
)

test_that("Initialize `CompositeScaledBernsteinFunction`", {
  expect_s4_class(
    bf_composite_scaled_bf,
    class = "CompositeScaledBernsteinFunction"
  )

  expect_error(
    ScaledBernsteinFunction(
      cscale = -1,
      ConstantBernsteinFunction(constant = 1)
    )
  )
  expect_error(
    ScaledBernsteinFunction(
      cscale = Inf,
      ConstantBernsteinFunction(constant = 1)
    )
  )
  expect_error(
    ScaledBernsteinFunction(
      cscale = NA,
      ConstantBernsteinFunction(constant = 1)
    )
  )
  expect_error(
    ScaledBernsteinFunction(
      cscale = NaN,
      ConstantBernsteinFunction(constant = 1)
    )
  )
})

cscale <- sqrt(2)
x <- seq(0, 10, by = 0.25)
actual_fn <- function(x, original_cscale, alpha) {
  (original_cscale * x)^alpha
}

test_that("`calcIterativeDifference` calculates expected values", {
  expect_equal(
    calcIterativeDifference(bf_composite_scaled_bf, x),
    actual_fn(
      x,
      original_cscale = bf_composite_scaled_bf@cscale,
      alpha = bf_composite_scaled_bf@original@alpha
    )
  )

  expect_equal(
    calcIterativeDifference(bf_composite_scaled_bf, x),
    calcValue(bf_composite_scaled_bf, x)
  )

  expect_equal(
    calcIterativeDifference(bf_composite_scaled_bf, x, cscale = cscale),
    actual_fn(
      cscale * x,
      original_cscale = bf_composite_scaled_bf@cscale,
      alpha = bf_composite_scaled_bf@original@alpha
    )
  )
})

d <- 7

test_that("`calcExShockSizeArrivalIntensities` calculates expected values", {
  expect_equal(
    calcExShockSizeArrivalIntensities(bf_composite_scaled_bf, d),
    calc_ex_shock_size_arrival_intensities_naive(
      actual_fn, d,
      original_cscale = bf_composite_scaled_bf@cscale,
      alpha = bf_composite_scaled_bf@original@alpha
    )
  )

  expect_equal(
    calcExShockSizeArrivalIntensities(
      bf_composite_scaled_bf, d, cscale = cscale
    ),
    calc_ex_shock_size_arrival_intensities_naive(
      actual_fn, d,
      original_cscale = bf_composite_scaled_bf@cscale,
      alpha = bf_composite_scaled_bf@original@alpha,
      cscale = cscale
    )
  )

  expect_equal(
    calcExShockSizeArrivalIntensities(
      bf_composite_scaled_bf, d, cscale = cscale
    ),
    calcExShockSizeArrivalIntensities(
      bf_composite_scaled_bf, d,
      cscale = cscale,
      method = "levy",
      tolerance = testthat_tolerance()
    )
  )

  expect_equal(
    calcExShockSizeArrivalIntensities(
      bf_composite_scaled_bf, d, cscale = cscale
    ),
    calcExShockSizeArrivalIntensities(
      bf_composite_scaled_bf, d,
      cscale = cscale,
      method = "stieltjes", tolerance = testthat_tolerance()
    )
  )
})

test_that("`calcMDCMGeneratorMatrix` calculates expected values", {
  expect_equal(
    calcMDCMGeneratorMatrix(bf_composite_scaled_bf, d),
    mdcm_generator_matrix_naive(
      actual_fn, d,
      original_cscale = bf_composite_scaled_bf@cscale,
      alpha = bf_composite_scaled_bf@original@alpha
    )
  )

  expect_equal(
    calcMDCMGeneratorMatrix(bf_composite_scaled_bf, d, cscale = cscale),
    mdcm_generator_matrix_naive(
      actual_fn, d,
      original_cscale = bf_composite_scaled_bf@cscale,
      alpha = bf_composite_scaled_bf@original@alpha,
      cscale = cscale
    )
  )

  expect_equal(
    calcMDCMGeneratorMatrix(bf_composite_scaled_bf, d, cscale = cscale),
    calcMDCMGeneratorMatrix(
      bf_composite_scaled_bf, d,
      cscale = cscale,
      method = "levy", tolerance = testthat_tolerance()
    )
  )

  expect_equal(
    calcMDCMGeneratorMatrix(bf_composite_scaled_bf, d, cscale = cscale),
    calcMDCMGeneratorMatrix(
      bf_composite_scaled_bf, d,
      cscale = cscale,
      method = "stieltjes", tolerance = testthat_tolerance()
    )
  )
})
