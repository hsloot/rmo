context("Primitive Rcpp generators")
use_seed <- 1632L
n <- 1e5L

test_that("RExpGenerator works as expected", {
	args <- list("rate" = 0.5)
  expect_equal_sampling_result("rexp", "Rcppmo_th_rexp",
    args, n, use_seed)
})

test_that("RUnifGenerator works as expected", {
	args <- list()
  expect_equal_sampling_result("runif", "Rcppmo_th_unif",
    args, n, use_seed)
})
