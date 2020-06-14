context("Parameter")

test_that("conversion from ex_intensities to intensities", {
  expect_equal(
    ex_intensities2intensities(
      ex_intensities_linear(2L, scale=1)
    ),
    c(1, 1, 0)
  )

  expect_equal(
    ex_intensities2intensities(
      ex_intensities_constant(2L, constant=1)
    ),
    c(0, 0, 1)
  )

  expect_equal(
    ex_intensities2intensities(
      ex_intensities_linear(3L, scale=1)
    ),
    c(1, 1, 0, 1, 0, 0, 0)
  )

  expect_equal(
    ex_intensities2intensities(
      ex_intensities_constant(3L, constant=1)
    ),
    c(0, 0, 0, 0, 0, 0, 1)
  )
})
