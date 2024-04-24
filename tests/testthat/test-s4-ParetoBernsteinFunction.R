bf_pareto <- ParetoBernsteinFunction(alpha = 0.5365548, x0 = 1e-2)

test_that("Initialize `ParetoBernsteinFunction`", {
  expect_s4_class(bf_pareto, class = "ParetoBernsteinFunction")

  expect_error(ParetoBernsteinFunction(alpha = 0, x0 = 1e-4))
  expect_error(ParetoBernsteinFunction(alpha = -1, x0 = 1e-4))
  expect_error(ParetoBernsteinFunction(alpha = 0.4, x0 = -1e-4))
  expect_error(ParetoBernsteinFunction(alpha = -1, x0 = -1e-4))
  expect_error(ParetoBernsteinFunction(alpha = c(0.4, 0.3), x0 = 1e-4))
  expect_error(ParetoBernsteinFunction(alpha = 2, x0 = 1e-4))
  expect_error(ParetoBernsteinFunction(alpha = NA, x0 = 1e-4))
  expect_error(ParetoBernsteinFunction(alpha = NaN, x0 = 1e-4))

  expect_error(validObject(ParetoBernsteinFunction(alpha = 0.4)))
  expect_error(validObject(ParetoBernsteinFunction(x0 = 1e-3)))
})

cscale <- sqrt(2)
x <- seq(0, 10, by = 0.25)
actual_fn <- function(x, alpha, x0) {
  1 - exp(-x0 * x) +
    (x * x0)^(alpha) *
      pgamma(x0 * x, 1 - alpha, lower = FALSE) *
      gamma(1 - alpha)
}

test_that("`valueOf` calculates expected values", {
  expect_equal(
    valueOf(bf_pareto, x),
    actual_fn(x, bf_pareto@alpha, bf_pareto@x0)
  )

  expect_equal(
    valueOf(bf_pareto, x),
    valueOf0(bf_pareto, x)
  )

  expect_equal(
    valueOf(bf_pareto, x, cscale = cscale),
    actual_fn(cscale * x, bf_pareto@alpha, bf_pareto@x0)
  )
})

d <- 7

test_that("`calcExShockSizeArrivalIntensities` calculates expected values", {
  expect_equal(
    calcExShockSizeArrivalIntensities(bf_pareto, d),
    calc_ex_shock_size_arrival_intensities_naive(
      actual_fn, d,
      alpha = bf_pareto@alpha,
      x0 = bf_pareto@x0
    )
  )

  expect_equal(
    calcExShockSizeArrivalIntensities(bf_pareto, d, cscale = cscale),
    calc_ex_shock_size_arrival_intensities_naive(
      actual_fn, d,
      alpha = bf_pareto@alpha,
      x0 = bf_pareto@x0,
      cscale = cscale
    )
  )

  expect_equal(
    calcExShockSizeArrivalIntensities(bf_pareto, d, cscale = cscale),
    calcExShockSizeArrivalIntensities(
      bf_pareto, d,
      cscale = cscale,
      method = "levy",
      tolerance = testthat_tolerance()
    )
  )
})

test_that("`exQMatrix` calculates expected values", {
  expect_equal(
    exQMatrix(bf_pareto, d),
    ex_qmatrix_naive(
      actual_fn, d,
      alpha = bf_pareto@alpha,
      x0 = bf_pareto@x0
    )
  )

  expect_equal(
    exQMatrix(bf_pareto, d, cscale = cscale),
    ex_qmatrix_naive(
      actual_fn, d,
      cscale = cscale,
      alpha = bf_pareto@alpha,
      x0 = bf_pareto@x0
    )
  )

  expect_equal(
    exQMatrix(bf_pareto, d, cscale = cscale),
    exQMatrix(
      bf_pareto, d,
      cscale = cscale,
      method = "levy", tolerance = testthat_tolerance()
    )
  )
})
