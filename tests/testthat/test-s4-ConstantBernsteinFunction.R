bf_constant <- ConstantBernsteinFunction(constant = 1.416005)

test_that("Initialize `ConstantBernsteinFunction`", {
  expect_s4_class(bf_constant, "ConstantBernsteinFunction")

  expect_error(ConstantBernsteinFunction(constant = -1))
  expect_error(ConstantBernsteinFunction(constant = c(4, 1)))
  expect_error(ConstantBernsteinFunction(constant = Inf))
  expect_error(ConstantBernsteinFunction(constant = NA))
  expect_error(ConstantBernsteinFunction(constant = NaN))
})

cscale <- sqrt(2)
x <- seq(0, 10, by = 0.25)
actual_fn <- function(x, constant) {
  ifelse(0 < x, constant, 0)
}

test_that("`calcIterativeDifference` calculates expected values", {
  expect_equal(
    calcIterativeDifference(bf_constant, x),
    actual_fn(x, bf_constant@constant)
  )

  expect_equal(
    calcIterativeDifference(bf_constant, x),
    calcValue(bf_constant, x)
  )

  expect_equal(
    calcIterativeDifference(bf_constant, x, cscale = cscale),
    actual_fn(cscale * x, bf_constant@constant)
  )
})

d <- 7

test_that("`calcExShockSizeArrivalIntensities` calculates expected values", {
  expect_equal(
    calcExShockSizeArrivalIntensities(bf_constant, d),
    calc_ex_shock_size_arrival_intensities_naive(
      actual_fn, d,
      constant = bf_constant@constant
    )
  )

  expect_equal(
    calcExShockSizeArrivalIntensities(bf_constant, d, cscale = cscale),
    calc_ex_shock_size_arrival_intensities_naive(
      actual_fn, d,
      constant = bf_constant@constant,
      cscale = cscale
    )
  )
})

test_that("`calcMDCMGeneratorMatrix` calculates expected values", {
  expect_equal(
    calcMDCMGeneratorMatrix(bf_constant, d),
    mdcm_generator_matrix_naive(
      actual_fn, d,
      constant = bf_constant@constant
    )
  )

  expect_equal(
    calcMDCMGeneratorMatrix(bf_constant, d, cscale = cscale),
    mdcm_generator_matrix_naive(
      actual_fn, d,
      constant = bf_constant@constant,
      cscale = cscale
    )
  )
})
