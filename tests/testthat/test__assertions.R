context("Custom assertions")

test_that("`is_mo_parameter` assertion works as intended", {
  expect_true(assert_that(is_mo_parameter(c(0, 0, 1))))
  expect_true(assert_that(is_mo_parameter(c(1, 1, 0))))
  expect_true(assert_that(is_mo_parameter(c(1, 1, 1))))

  expect_error(assert_that(is_mo_parameter(c(0, 0, 0))),
    regexp=sprintf(ERR_MARGINRATE_NOT_POS, "*"))
})

test_that("`is_exmo_parameter` assertion works as intended", {
  expect_true(assert_that(is_exmo_parameter(c(0, 1))))
  expect_true(assert_that(is_exmo_parameter(c(1, 0))))
  expect_true(assert_that(is_exmo_parameter(c(1, 1))))

  expect_error(assert_that(is_exmo_parameter(c(0, 0))),
    regexp=sprintf(ERR_MARGINRATE_NOT_POS, "*"))
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

test_that("`is_rjump_name` assertion works as intended", {
  expect_true(assert_that(is_rjump_name("rexp")))

  expect_error(assert_that(is_rjump_name("rnorm")),
    regexp=sprintf(ERR_NOT_RJUMP_NAME, "*"))
})

test_that("`is_rjump_arg_list` assertion works as intended", {
  expect_true(assert_that(is_rjump_arg_list("rexp", list("rate"=0.5))))

  expect_error(assert_that(is_rjump_arg_list("rexp", list("scale"=2))),
    regexp=sprintf(ERR_NOT_RJUMP_ARGS, "*", "[.]*"))
})
