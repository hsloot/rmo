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

test_that("`valueOf` calculates expected values", {
  expect_equal(
    valueOf(bf_constant, x),
    actual_fn(x, bf_constant@constant)
  )

  expect_equal(
    valueOf(bf_constant, x),
    valueOf0(bf_constant, x)
  )

  expect_equal(
    valueOf(bf_constant, x, cscale = cscale),
    actual_fn(cscale * x, bf_constant@constant)
  )
})

d <- 7

test_that("`exIntensities` calculates expected values", {
  expect_equal(
    exIntensities(bf_constant, d),
    ex_intensities_naive(
      actual_fn, d,
      constant = bf_constant@constant
    )
  )

  expect_equal(
    exIntensities(bf_constant, d, cscale = cscale),
    ex_intensities_naive(
      actual_fn, d,
      constant = bf_constant@constant,
      cscale = cscale
    )
  )
})

test_that("`exQMatrix` calculates expected values", {
  expect_equal(
    exQMatrix(bf_constant, d),
    ex_qmatrix_naive(
      actual_fn, d,
      constant = bf_constant@constant
    )
  )

  expect_equal(
    exQMatrix(bf_constant, d, cscale = cscale),
    ex_qmatrix_naive(
      actual_fn, d,
      constant = bf_constant@constant,
      cscale = cscale
    )
  )
})
