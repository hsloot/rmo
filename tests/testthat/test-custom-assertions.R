## TODO: Implement more tests for higher dimensions


test_that("`is_positive_number` assertion works as intended", {
  expect_true(assert_that(is_positive_number(0.5)))
  expect_true(assert_that(is_positive_number(1)))
  expect_true(assert_that(is_positive_number(Inf)))

  expect_error(
    assert_that(is_positive_number(0)),
    regexp=sprintf(ERR_X_NOT_Y, "*", "positive number"))
  expect_error(
    assert_that(is_positive_number(-1)),
    regexp=sprintf(ERR_X_NOT_Y, "*", "positive number"))
  expect_error(
    assert_that(is_positive_number(c(1, 2))),
    regexp=sprintf(ERR_X_NOT_Y, "*", "positive number"))
})

test_that("`is_nonnegative_number` assertion works as intended", {
  expect_true(assert_that(is_nonnegative_number(0)))
  expect_true(assert_that(is_nonnegative_number(0.5)))
  expect_true(assert_that(is_nonnegative_number(1)))
  expect_true(assert_that(is_nonnegative_number(Inf)))

  expect_error(
    assert_that(is_nonnegative_number(-1)),
    regexp=sprintf(ERR_X_NOT_Y, "*", "non-negative number"))
  expect_error(
    assert_that(is_nonnegative_number(c(1, 2))),
    regexp=sprintf(ERR_X_NOT_Y, "*", "non-negative number"))
})

test_that("`is_nonnegative_vector` assertion works as intended", {
  expect_true(assert_that(is_nonnegative_vector(0)))
  expect_true(assert_that(is_nonnegative_vector(c(0, 0))))
  expect_true(assert_that(is_nonnegative_vector(c(0, rep(1, 5)))))

  expect_error(
    assert_that(is_nonnegative_vector(-1)),
    regexp=sprintf(ERR_X_NOT_Y, "*", "non-negative vector"))
  expect_error(
    assert_that(is_nonnegative_vector(c(rep(1, 5), -1))),
    regexp=sprintf(ERR_X_NOT_Y, "*", "non-negative vector"))
})

test_that("`is_nonzerovector` assertion works as intended", {
  expect_true(assert_that(is_nonzero_vector(1)))
  expect_true(assert_that(is_nonzero_vector(c(1, 0))))
  expect_true(assert_that(is_nonzero_vector(c(-1, 0, 1))))

  expect_error(
    assert_that(is_nonzero_vector(0)),
    regexp=sprintf(ERR_X_NOT_Y, "*", "non-zero vector"))
  expect_error(
    assert_that(is_nonzero_vector(rep(0, 5))),
    regexp=sprintf(ERR_X_NOT_Y, "*", "non-zero vector"))
})

test_that("`has_length` assertion works as intended", {
  expect_true(assert_that(1 %has_length% 1))
  expect_true(assert_that(rep(1, 5) %has_length% 5))
  expect_true(assert_that(c("a", 1, TRUE) %has_length% 3))

  expect_error(assert_that(c(0, 1, 3, 4) %has_length% 3))
  expect_error(assert_that(rep(0, 7) %has_length% 9))
})
