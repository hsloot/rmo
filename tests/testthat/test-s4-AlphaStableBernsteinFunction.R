bf_alpha_stable <- AlphaStableBernsteinFunction(alpha = 0.6132243)

test_that("Initialize `AlphaStableBernsteinFunction`", {
  expect_s4_class(bf_alpha_stable, class = "AlphaStableBernsteinFunction")

  expect_error(AlphaStableBernsteinFunction(alpha = -0.5))
  expect_error(AlphaStableBernsteinFunction(alpha = 0))
  expect_error(AlphaStableBernsteinFunction(alpha = 1))
  expect_error(AlphaStableBernsteinFunction(alpha = 1.5))
  expect_error(AlphaStableBernsteinFunction(alpha = -1))
  expect_error(AlphaStableBernsteinFunction(alpha = c(1, 2)))
  expect_error(AlphaStableBernsteinFunction(alpha = NA))
  expect_error(AlphaStableBernsteinFunction(alpha = NaN))
})

cscale <- sqrt(2)
x <- seq(0, 10, by = 0.25)
actual_fn <- function(x, alpha) {
  x^alpha
}

test_that("`calcIterativeDifference` calculates expected values", {
  expect_equal(
    calcIterativeDifference(bf_alpha_stable, x),
    actual_fn(x, bf_alpha_stable@alpha)
  )

  expect_equal(
    calcIterativeDifference(bf_alpha_stable, x),
    valueOf0(bf_alpha_stable, x)
  )

  expect_equal(
    calcIterativeDifference(bf_alpha_stable, x, cscale = cscale),
    actual_fn(cscale * x, bf_alpha_stable@alpha)
  )

  expect_error(
    calcIterativeDifference(
      AlphaStableBernsteinFunction(log2(2 - 1e-4)),
      x,
      difference_order = 1L
    ), NA
  )
  expect_error(
    calcIterativeDifference(
      AlphaStableBernsteinFunction(log2(2 - (1 - 1e-4))),
      x,
      difference_order = 1L
    ), NA
  )
})

d <- 7

test_that("`calcExShockSizeArrivalIntensities` calculates expected values", {
  expect_equal(
    calcExShockSizeArrivalIntensities(bf_alpha_stable, d),
    calc_ex_shock_size_arrival_intensities_naive(
      actual_fn, d, alpha = bf_alpha_stable@alpha
    )
  )

  expect_equal(
    calcExShockSizeArrivalIntensities(bf_alpha_stable, d, cscale = cscale),
    calc_ex_shock_size_arrival_intensities_naive(
      actual_fn, d,
      alpha = bf_alpha_stable@alpha,
      cscale = cscale
    )
  )

  expect_equal(
    calcExShockSizeArrivalIntensities(bf_alpha_stable, d, cscale = cscale),
    calcExShockSizeArrivalIntensities(
      bf_alpha_stable, d,
      cscale = cscale,
      method = "levy",
      tolerance = testthat_tolerance()
    )
  )

  expect_equal(
    calcExShockSizeArrivalIntensities(bf_alpha_stable, d, cscale = cscale),
    calcExShockSizeArrivalIntensities(
      bf_alpha_stable, d,
      cscale = cscale,
      method = "stieltjes", tolerance = testthat_tolerance()
    )
  )
})

test_that("`calcMDCMGeneratorMatrix` calculates expected values", {
  expect_equal(
    calcMDCMGeneratorMatrix(bf_alpha_stable, d),
    ex_qmatrix_naive(
      actual_fn, d,
      alpha = bf_alpha_stable@alpha
    )
  )

  expect_equal(
    calcMDCMGeneratorMatrix(bf_alpha_stable, d, cscale = cscale),
    ex_qmatrix_naive(
      actual_fn, d,
      cscale = cscale,
      alpha = bf_alpha_stable@alpha
    )
  )

  expect_equal(
    calcMDCMGeneratorMatrix(bf_alpha_stable, d, cscale = cscale),
    calcMDCMGeneratorMatrix(
      bf_alpha_stable, d,
      cscale = cscale,
      method = "levy", tolerance = testthat_tolerance()
    )
  )

  expect_equal(
    calcMDCMGeneratorMatrix(bf_alpha_stable, d, cscale = cscale),
    calcMDCMGeneratorMatrix(
      bf_alpha_stable, d,
      cscale = cscale,
      method = "stieltjes", tolerance = testthat_tolerance()
    )
  )
})
