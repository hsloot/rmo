context("Custom assertions")

test_that("`is_mo_parameter` assertion works as intended", {
  d <- 2L
  expect_true(assert_that(is_mo_parameter(d, intensities=c(0, 0, 1))))
  expect_true(assert_that(is_mo_parameter(d, intensities=c(1, 1, 0))))
  expect_true(assert_that(is_mo_parameter(d, intensities=c(1, 1, 1))))

  expect_error(assert_that(is_mo_parameter(d, intensities=c(0, 0, 0))),
    regexp=sprintf(ERR_NOT_MO_INTENSITIES, "*", d))
})

test_that("`is_ex_mo_parameter` assertion works as intended", {
  d <- 2L
  expect_true(assert_that(is_ex_mo_parameter(d, ex_intensities=c(0, 1))))
  expect_true(assert_that(is_ex_mo_parameter(d, ex_intensities=c(1, 0))))
  expect_true(assert_that(is_ex_mo_parameter(d, ex_intensities=c(1, 1))))

  expect_error(assert_that(is_ex_mo_parameter(d, ex_intensities=c(0, 0))),
    regexp=sprintf(ERR_NOT_EX_MO_INTENSITIES, "*", d))
})

test_that("`is_positive_number` assertion works as intended", {
  expect_true(assert_that(is_positive_number(0.5)))
  expect_true(assert_that(is_positive_number(1)))
  expect_true(assert_that(is_positive_number(Inf)))

  expect_error(assert_that(is_positive_number(0)),
    regexp=sprintf(ERR_NOT_SCALAR_X_NUMBER, "*", "positive"))
  expect_error(assert_that(is_positive_number(-1)),
    regexp=sprintf(ERR_NOT_SCALAR_X_NUMBER, "*", "positive"))
  expect_error(assert_that(is_positive_number(c(1, 2))),
    regexp=sprintf(ERR_NOT_SCALAR_X_NUMBER, "*", "positive"))
})

test_that("`is_nonnegative_number` assertion works as intended", {
  expect_true(assert_that(is_nonnegative_number(0)))
  expect_true(assert_that(is_nonnegative_number(0.5)))
  expect_true(assert_that(is_nonnegative_number(1)))
  expect_true(assert_that(is_nonnegative_number(Inf)))

  expect_error(assert_that(is_nonnegative_number(-1)),
    regexp=sprintf(ERR_NOT_SCALAR_X_NUMBER, "*", "non-negative"))
  expect_error(assert_that(is_nonnegative_number(c(1, 2))),
    regexp=sprintf(ERR_NOT_SCALAR_X_NUMBER, "*", "non-negative"))
})

test_that("`is_dimension` assertion works as intended", {
  expect_true(assert_that(is_dimension(2L)))
  expect_true(assert_that(is_dimension(15)))
  expect_true(assert_that(is_dimension(31L)))
  expect_true(assert_that(is_dimension(32L)))

  expect_error(assert_that(is_dimension(-1L)),
    regexp=sprintf(ERR_NOT_DIMENSION, "*"))
  expect_error(assert_that(is_dimension(0)),
    regexp=sprintf(ERR_NOT_DIMENSION, "*"))
  expect_error(assert_that(is_dimension("2")),
    regexp=sprintf(ERR_NOT_DIMENSION, "*"))
  expect_error(assert_that(is_dimension(c(2L, 3L))),
    regexp=sprintf(ERR_NOT_DIMENSION, "*"))
})

test_that("`is_32bit_complient_dimension` assertion works as intended", {
  expect_true(assert_that(is_32bit_complient_dimension(2L)))
  expect_true(assert_that(is_32bit_complient_dimension(15)))
  expect_true(assert_that(is_32bit_complient_dimension(31L)))

  expect_error(assert_that(is_32bit_complient_dimension(-1L)),
    regexp=sprintf(ERR_NOT_32BIT_COMPLIENT_DIMENSION, "*"))
  expect_error(assert_that(is_32bit_complient_dimension(0)),
    regexp=sprintf(ERR_NOT_32BIT_COMPLIENT_DIMENSION, "*"))
  expect_error(assert_that(is_32bit_complient_dimension("2")),
    regexp=sprintf(ERR_NOT_32BIT_COMPLIENT_DIMENSION, "*"))
  expect_error(assert_that(is_32bit_complient_dimension(c(2L, 3L))),
    regexp=sprintf(ERR_NOT_32BIT_COMPLIENT_DIMENSION, "*"))
  expect_error(assert_that(is_32bit_complient_dimension(32L)),
    regexp=sprintf(ERR_NOT_32BIT_COMPLIENT_DIMENSION, "*"))
})

test_that("`is_rjump_name` assertion works as intended", {
  expect_true(assert_that(is_rjump_name("rexp")))

  expect_error(assert_that(is_rjump_name("rnorm")),
    regexp=sprintf(ERR_NOT_RJUMP_NAME, "*"))
})

test_that("`is_rjump_param` assertion works as intended", {
  expect_true(assert_that(is_rjump_param("rexp", list("rate"=0.5))))

  expect_error(assert_that(is_rjump_param("rexp", list("scale"=2))),
    regexp=sprintf(ERR_NOT_RJUMP_ARGS, "*", "[.]*"))

  expect_error(assert_that(is_rjump_param("rexp", list("rate"=-1))),
    regexp=sprintf(ERR_NOT_RJUMP_ARGS, "*", "[.]*"))
})
