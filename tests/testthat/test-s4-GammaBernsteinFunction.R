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

test_that("`valueOf` calculates expected values", {
  expect_equal(
    valueOf(bf_gamma, x),
    actual_fn(x, bf_gamma@a)
  )

  expect_equal(
    valueOf(bf_gamma, x),
    valueOf0(bf_gamma, x)
  )

  expect_equal(
    valueOf(bf_gamma, x, cscale = cscale),
    actual_fn(cscale * x, bf_gamma@a)
  )
})

d <- 7

test_that("`exIntensities` calculates expected values", {
  expect_equal(
    exIntensities(bf_gamma, d),
    ex_intensities_naive(
      actual_fn, d,
      a = bf_gamma@a
    )
  )

  expect_equal(
    exIntensities(bf_gamma, d, cscale = cscale),
    ex_intensities_naive(
      actual_fn, d,
      a = bf_gamma@a,
      cscale = cscale
    )
  )

  expect_equal(
    exIntensities(bf_gamma, d, cscale = cscale),
    exIntensities(
      bf_gamma, d,
      cscale = cscale,
      method = "levy",
      tolerance = testthat_tolerance()
    )
  )

  expect_equal(
    exIntensities(bf_gamma, d, cscale = cscale),
    exIntensities(
      bf_gamma, d,
      cscale = cscale,
      method = "stieltjes", tolerance = testthat_tolerance()
    )
  )
})

test_that("`exQMatrix` calculates expected values", {
  expect_equal(
    exQMatrix(bf_gamma, d),
    ex_qmatrix_naive(
      actual_fn, d,
      a = bf_gamma@a
    )
  )

  expect_equal(
    exQMatrix(bf_gamma, d, cscale = cscale),
    ex_qmatrix_naive(
      actual_fn, d,
      cscale = cscale,
      a = bf_gamma@a
    )
  )

  expect_equal(
    exQMatrix(bf_gamma, d, cscale = cscale),
    exQMatrix(
      bf_gamma, d,
      cscale = cscale,
      method = "levy", tolerance = testthat_tolerance()
    )
  )

  expect_equal(
    exQMatrix(bf_gamma, d, cscale = cscale),
    exQMatrix(
      bf_gamma, d,
      cscale = cscale,
      method = "stieltjes", tolerance = testthat_tolerance()
    )
  )
})
