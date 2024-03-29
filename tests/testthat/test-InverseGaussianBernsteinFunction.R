bf_inverse_gaussian <- InverseGaussianBernsteinFunction(eta = 1.133682)

test_that("Initialize `InverseGaussianBernsteinFunction`", {
  expect_s4_class(
    bf_inverse_gaussian,
    class = "InverseGaussianBernsteinFunction"
  )

  expect_error(InverseGaussianBernsteinFunction(alpha = -0.5))
  expect_error(InverseGaussianBernsteinFunction(alpha = 0))
  expect_error(InverseGaussianBernsteinFunction(alpha = Inf))
  expect_error(InverseGaussianBernsteinFunction(alpha = -1))
  expect_error(InverseGaussianBernsteinFunction(alpha = c(1, 2)))
  expect_error(InverseGaussianBernsteinFunction(alpha = NA))
  expect_error(InverseGaussianBernsteinFunction(alpha = NaN))
})

cscale <- sqrt(2)
x <- seq(0, 10, by = 0.25)
actual_fn <- function(x, eta) {
  sqrt(2 * x + eta^2) - eta
}

test_that("`valueOf` calculates expected values", {
  expect_equal(
    valueOf(bf_inverse_gaussian, x),
    actual_fn(x, bf_inverse_gaussian@eta)
  )

  expect_equal(
    valueOf(bf_inverse_gaussian, x),
    valueOf0(bf_inverse_gaussian, x)
  )

  expect_equal(
    valueOf(bf_inverse_gaussian, x, cscale = cscale),
    actual_fn(cscale * x, bf_inverse_gaussian@eta)
  )
})

d <- 7

test_that("`exIntensities` calculates expected values", {
  expect_equal(
    exIntensities(bf_inverse_gaussian, d),
    ex_intensities_naive(
      actual_fn, d,
      eta = bf_inverse_gaussian@eta
    )
  )

  expect_equal(
    exIntensities(bf_inverse_gaussian, d, cscale = cscale),
    ex_intensities_naive(
      actual_fn, d,
      eta = bf_inverse_gaussian@eta,
      cscale = cscale
    )
  )

  expect_equal(
    exIntensities(bf_inverse_gaussian, d, cscale = cscale),
    exIntensities(
      bf_inverse_gaussian, d,
      cscale = cscale,
      method = "levy",
      tolerance = testthat_tolerance()
    )
  )

  expect_equal(
    exIntensities(bf_inverse_gaussian, d, cscale = cscale),
    exIntensities(
      bf_inverse_gaussian, d,
      cscale = cscale,
      method = "stieltjes", tolerance = testthat_tolerance()
    )
  )
})

test_that("`exQMatrix` calculates expected values", {
  expect_equal(
    exQMatrix(bf_inverse_gaussian, d),
    ex_qmatrix_naive(
      actual_fn, d,
      eta = bf_inverse_gaussian@eta
    )
  )

  expect_equal(
    exQMatrix(bf_inverse_gaussian, d, cscale = cscale),
    ex_qmatrix_naive(
      actual_fn, d,
      cscale = cscale,
      eta = bf_inverse_gaussian@eta
    )
  )

  expect_equal(
    exQMatrix(bf_inverse_gaussian, d, cscale = cscale),
    exQMatrix(
      bf_inverse_gaussian, d,
      cscale = cscale,
      method = "levy", tolerance = testthat_tolerance()
    )
  )

  expect_equal(
    exQMatrix(bf_inverse_gaussian, d, cscale = cscale),
    exQMatrix(
      bf_inverse_gaussian, d,
      cscale = cscale,
      method = "stieltjes", tolerance = testthat_tolerance()
    )
  )
})
