context("Custom assertions") ## TODO: Implement more tests for higher dimensions


test_that("`is_positive_number` assertion works as intended", {
  expect_true(assert_that(is_positive_number(0.5)))
  expect_true(assert_that(is_positive_number(1)))
  expect_true(assert_that(is_positive_number(Inf)))

  expect_error(assert_that(is_positive_number(0)),
    regexp=sprintf(ERR_X_NOT_Y, "*", "positive number"))
  expect_error(assert_that(is_positive_number(-1)),
    regexp=sprintf(ERR_X_NOT_Y, "*", "positive number"))
  expect_error(assert_that(is_positive_number(c(1, 2))),
    regexp=sprintf(ERR_X_NOT_Y, "*", "positive number"))
})

test_that("`is_nonnegative_number` assertion works as intended", {
  expect_true(assert_that(is_nonnegative_number(0)))
  expect_true(assert_that(is_nonnegative_number(0.5)))
  expect_true(assert_that(is_nonnegative_number(1)))
  expect_true(assert_that(is_nonnegative_number(Inf)))

  expect_error(assert_that(is_nonnegative_number(-1)),
    regexp=sprintf(ERR_X_NOT_Y, "*", "non-negative number"))
  expect_error(assert_that(is_nonnegative_number(c(1, 2))),
    regexp=sprintf(ERR_X_NOT_Y, "*", "non-negative number"))
})

test_that("`is_dimension` assertion works as intended", {
  expect_true(assert_that(is_dimension(2L)))
  expect_true(assert_that(is_dimension(15)))
  expect_true(assert_that(is_dimension(31L)))
  expect_true(assert_that(is_dimension(32L)))

  expect_error(assert_that(is_dimension(-1L)),
    regexp=sprintf(ERR_X_NOT_Y, "*", "dimension parameter"))
  expect_error(assert_that(is_dimension(0)),
    regexp=sprintf(ERR_X_NOT_Y, "*", "dimension parameter"))
  expect_error(assert_that(is_dimension("2")),
    regexp=sprintf(ERR_X_NOT_Y, "*", "dimension parameter"))
  expect_error(assert_that(is_dimension(c(2L, 3L))),
    regexp=sprintf(ERR_X_NOT_Y, "*", "dimension parameter"))
})

test_that("`is_32bit_compliant_dimension` assertion works as intended", {
  expect_true(assert_that(is_32bit_compliant_dimension(2L)))
  expect_true(assert_that(is_32bit_compliant_dimension(15)))
  expect_true(assert_that(is_32bit_compliant_dimension(31L)))

  expect_error(assert_that(is_32bit_compliant_dimension(-1L)),
    regexp=sprintf(ERR_X_NOT_Y, "*", "32bit compliant dimension parameter"))
  expect_error(assert_that(is_32bit_compliant_dimension(0)),
    regexp=sprintf(ERR_X_NOT_Y, "*", "32bit compliant dimension parameter"))
  expect_error(assert_that(is_32bit_compliant_dimension("2")),
    regexp=sprintf(ERR_X_NOT_Y, "*", "32bit compliant dimension parameter"))
  expect_error(assert_that(is_32bit_compliant_dimension(c(2L, 3L))),
    regexp=sprintf(ERR_X_NOT_Y, "*", "32bit compliant dimension parameter"))
  expect_error(assert_that(is_32bit_compliant_dimension(32L)),
    regexp=sprintf(ERR_X_NOT_Y, "*", "32bit compliant dimension parameter"))
})


test_that("`is_mo_parameter` assertion works as intended", {
  d <- 2L
  expect_true(assert_that(is_mo_parameter(d, intensities=c(0, 0, 1))))
  expect_true(assert_that(is_mo_parameter(d, intensities=c(1, 1, 0))))
  expect_true(assert_that(is_mo_parameter(d, intensities=c(1, 1, 1))))

  expect_error(assert_that(is_mo_parameter(d, intensities=c(0, 0, 0))),
    regexp=sprintf(ERR_X_NOT_Y_FOR_Z, "*", "MO parameter", "dimension d"))
  expect_error(assert_that(is_mo_parameter(d, intensities=c(-1, 0, 0))),
    regexp=sprintf(ERR_X_NOT_Y_FOR_Z, "*", "MO parameter", "dimension d"))
  expect_error(assert_that(is_mo_parameter(d, intensities=c(1, 1))),
    regexp=sprintf(ERR_X_NOT_Y_FOR_Z, "*", "MO parameter", "dimension d"))
  expect_error(assert_that(is_mo_parameter(d, intensities=c("1", "1", "1"))),
    regexp=sprintf(ERR_X_NOT_Y_FOR_Z, "*", "MO parameter", "dimension d"))
})

test_that("`is_ex_mo_parameter` assertion works as intended", {
  d <- 2L
  expect_true(assert_that(is_ex_mo_parameter(d, ex_intensities=c(0, 1))))
  expect_true(assert_that(is_ex_mo_parameter(d, ex_intensities=c(1, 0))))
  expect_true(assert_that(is_ex_mo_parameter(d, ex_intensities=c(1, 1))))

  expect_error(assert_that(is_ex_mo_parameter(d, ex_intensities=c(0, 0))),
    regexp=sprintf(ERR_X_NOT_Y_FOR_Z, "*", "exMO parameter", "dimension d"))
  expect_error(assert_that(is_ex_mo_parameter(d, ex_intensities=c(-1, 0))),
    regexp=sprintf(ERR_X_NOT_Y_FOR_Z, "*", "exMO parameter", "dimension d"))
  expect_error(assert_that(is_ex_mo_parameter(d, ex_intensities=c(1, 1, 1))),
    regexp=sprintf(ERR_X_NOT_Y_FOR_Z, "*", "exMO parameter", "dimension d"))
  expect_error(assert_that(is_ex_mo_parameter(d, ex_intensities=c("1", "1"))),
    regexp=sprintf(ERR_X_NOT_Y_FOR_Z, "*", "exMO parameter", "dimension d"))
})

test_that("`is_cuadras_auge_parameter` assertion works as intended", {
  expect_true(assert_that(is_cuadras_auge_parameter(2L, 0, 0.5)))
  expect_true(assert_that(is_cuadras_auge_parameter(6L, 0.5, 0)))
  expect_true(assert_that(is_cuadras_auge_parameter(3L, 1.5, 0.5)))
})


test_that("`is_rjump_name` assertion works as intended", {
  expect_true(assert_that(is_rjump_name("rexp")))

  expect_error(assert_that(is_rjump_name("rnorm")),
    regexp=sprintf(ERR_X_NOT_Y, "*", "allowed cpp jump distribution"))
})

test_that("`is_rjump_parameter` assertion works as intended", {
  expect_true(assert_that(is_rjump_parameter(
    rjump_name="rexp", rjump_arg_list=list("rate"=0.5))))

  expect_error(assert_that(is_rjump_parameter(
    rjump_name="rexp", rjump_arg_list=list("scale"=2))),
    regexp=sprintf(ERR_X_NOT_Y_FOR_Z, "*", "argument list", "*"))

  expect_error(assert_that(is_rjump_parameter(
    rjump_name="rexp", rjump_arg_list=list("rate"=-1))),
    regexp=sprintf(ERR_X_NOT_Y_FOR_Z, "*", "argument list", "*"))
})
