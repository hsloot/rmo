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

test_that("RIntGenerator works as expected", {
  assign("sample_int_base_0",
    function(n, probabilities) {
      sapply(1:n, function(x) -1+sample.int(
        n=length(probabilities),
        size=1, prob = probabilities, replace=FALSE)
      )
  }, env = globalenv())
  probabilities <- c(8, 7, 3, 10, 6, 1, 2, 9, 5, 4)
  args <- list("probabilities" = probabilities)
  expect_equal_sampling_result("Rcppmo_th_int", "sample_int_base_0",
    args, n, use_seed)

  probabilities <- probabilities / sum(probabilities)
  args <- list("probabilities" = probabilities)
  expect_equal_sampling_result("Rcppmo_th_int", "sample_int_base_0",
    args, n, use_seed)

  probabilities <- rep(1, 10) / 10
  args <- list("probabilities" = probabilities)
  expect_equal_sampling_result("Rcppmo_th_int", "sample_int_base_0",
    args, n, use_seed)

  probabilities <- c(10, 20, 0.1)
  args <- list("probabilities" = probabilities)
  expect_equal_sampling_result("Rcppmo_th_int", "sample_int_base_0",
    args, n, use_seed)
})

