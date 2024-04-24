bf_linear <- LinearBernsteinFunction(scale = 1.027499)

test_that("Initialize `LinearBernsteinFunction`", {
  expect_s4_class(bf_linear, "LinearBernsteinFunction")

  expect_error(LinearBernsteinFunction(scale = -1))
  expect_error(LinearBernsteinFunction(scale = c(1, 2)))
  expect_error(LinearBernsteinFunction(scale = Inf))
  expect_error(LinearBernsteinFunction(scale = NA))
  expect_error(LinearBernsteinFunction(scale = NaN))
})

cscale <- sqrt(2)
x <- seq(0, 10, by = 0.25)
actual_fn <- function(x, scale) {
  x * scale
}

test_that("`valueOf` calculates expected values", {
  expect_equal(
    valueOf(bf_linear, x),
    actual_fn(x, bf_linear@scale)
  )

  expect_equal(
    valueOf(bf_linear, x),
    valueOf0(bf_linear, x)
  )

  expect_equal(
    valueOf(bf_linear, x, cscale = cscale),
    actual_fn(cscale * x, bf_linear@scale)
  )
})

d <- 7

test_that("`calcExShockSizeArrivalIntensities` calculates expected values", {
  expect_equal(
    calcExShockSizeArrivalIntensities(bf_linear, d),
    calc_ex_shock_size_arrival_intensities_naive(
      actual_fn, d,
      scale = bf_linear@scale
    )
  )

  expect_equal(
    calcExShockSizeArrivalIntensities(bf_linear, d, cscale = cscale),
    calc_ex_shock_size_arrival_intensities_naive(
      actual_fn, d,
      scale = bf_linear@scale,
      cscale = cscale
    )
  )
})

test_that("`calcMDCMGeneratorMatrix` calculates expected values", {
  expect_equal(
    calcMDCMGeneratorMatrix(bf_linear, d),
    ex_qmatrix_naive(
      actual_fn, d,
      scale = bf_linear@scale
    )
  )

  expect_equal(
    calcMDCMGeneratorMatrix(bf_linear, d, cscale = cscale),
    ex_qmatrix_naive(
      actual_fn, d,
      scale = bf_linear@scale,
      cscale = cscale
    )
  )
})
