context("sort functions")

test_that("`reverse_sort` works as expected", {
  a1 <- c(1, 5, 3, 4, 2, 1, 5)
  id1 <- seq_along(a1)
  a2 <- a1
  id2 <- id1
  mo_internal__reverse_sort(a1, id1)
  mo_internal__revsort_orig(a2, id2)
  expect_equal(a1, a2)
  expect_equal(id1, id2)
})


test_that("`reverse_sort` works as expected (all equal)", {
  a1 <- rep(1, 10)
  id1 <- seq_along(a1)
  a2 <- a1
  id2 <- id1
  mo_internal__reverse_sort(a1, id1)
  mo_internal__revsort_orig(a2, id2)
  expect_equal(a1, a2)
  expect_equal(id1, id2)
})

test_that("`sort_index` works as expected", {
  a <- c(1, 5, 3, 4, 2, 1, 5)
  expect_equal(mo_internal__sort_index(a), order(a)-1)
})

test_that("`sort_index` works as expected (all equal)", {
  a <- rep(1, 10)
  expect_equal(mo_internal__sort_index(a), order(a)-1)
})
