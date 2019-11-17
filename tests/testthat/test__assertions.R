context("Custom assertions")

test_that("mo intensities assertation succeeds", {
  expect_true(assert_that(is_mo_parameter(c(0, 0, 1))))
  expect_true(assert_that(is_mo_parameter(c(1, 1, 0))))
  expect_true(assert_that(is_mo_parameter(c(1, 1, 1))))

  expect_error(assert_that(is_mo_parameter(c(0, 0, 0))),
    regexp=gsub("%s", "*", ERR_MARGINRATE_NOT_POS))
})

test_that("exmo intensities assertation succeeds", {
  expect_true(assert_that(is_exmo_parameter(c(0, 1))))
  expect_true(assert_that(is_exmo_parameter(c(1, 0))))
  expect_true(assert_that(is_exmo_parameter(c(1, 1))))

  expect_error(assert_that(is_exmo_parameter(c(0, 0))),
    regexp=gsub("%s", "*", ERR_MARGINRATE_NOT_POS))
})
