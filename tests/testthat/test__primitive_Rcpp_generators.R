context("Primitive Rcpp generators")
use_seed <- 1632L
n <- 1e5L

test_that("RExpGenerator works as expected", {
  args <- list("rate" = 0.5)
  expect_equal_sampling_result("rexp", "Rcppmo_th_rexp",
    args, n, use_seed)
  args <- list("rate" = 2)
  expect_equal_sampling_result("rexp", "Rcppmo_th_rexp",
    args, n, use_seed)
})

test_that("FixedDblGenerator works as expected", {
  args <- list("value" = 1)
  expect_equal_sampling_result("rposval", "Rcppmo_th_fixeddbl",
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

test_that("RSamplewalker works as expected", {
  assign("perm_base_0",
    function(n, probabilities) {
      -1+sample.int(
        n=length(probabilities),
        size=n, prob = probabilities, replace=FALSE)
  }, env = globalenv())
  probabilities <- c(8, 7, 3, 10, 6, 1, 2, 9, 5, 4)
  n <- length(probabilities)
  args <- list("probabilities" = probabilities)
  expect_equal_sampling_result(
    "Rcppmo_th_perm", "perm_base_0",
    args, n, use_seed)

  probabilities <- probabilities / sum(probabilities)
  n <- length(probabilities)
  args <- list("probabilities" = probabilities)
  expect_equal_sampling_result(
    "Rcppmo_th_perm", "perm_base_0",
    args, n, use_seed)

  probabilities <- rep(1, 10) / 10
  n <- length(probabilities)
  args <- list("probabilities" = probabilities)
  expect_equal_sampling_result(
    "Rcppmo_th_perm", "perm_base_0",
    args, n, use_seed)

  probabilities <- c(10, 20, 0.1)
  n <- length(probabilities)
  args <- list("probabilities" = probabilities)
  expect_equal_sampling_result(
    "Rcppmo_th_perm", "perm_base_0",
    args, n, use_seed)
})


test_that("sample.int reimplementation works as expected", {
  # TODO: This is very (!) repetitive and should be refactored with
  # custom expectations.
  # TODO: There should be a better way to produce meaningful
  # parametrisations.
  # TODO: These tests should be refactored in a seperate test file.
  suppressWarnings(RNGkind(
    kind="Mersenne-Twister", normal.kind = "Inversion",
    sample.kind="Rejection"))

  args <- list("n" = 10, "size" = 10, "replace" = FALSE, "useHash" = FALSE)
  set.seed(use_seed)
  act <- do.call("Rcppmo_th_sample_int", args = args)
  set.seed(use_seed)
  exp <- do.call("sample.int", args = args)-1
  expect_equal(act, exp)

  args[c("n", "size", "replace")] <- c(1e2, 1e6, TRUE)
  set.seed(use_seed)
  act <- do.call("Rcppmo_th_sample_int", args = args)
  set.seed(use_seed)
  exp <- do.call("sample.int", args = args)-1
  expect_equal(act, exp)

  args["prob"] <- list(seq(1:100))
  args[c("n", "size", "replace")] <- c(100, 90, FALSE)
  set.seed(use_seed)
  act <- do.call("Rcppmo_th_sample_int", args = args)
  set.seed(use_seed)
  exp <- do.call("sample.int", args = args)-1
  expect_equal(act, exp)

  args["prob"] <- list(seq(1:100) / sum(1:100))
  args[c("n", "size", "replace")] <- c(100, 90, FALSE)
  set.seed(use_seed)
  act <- do.call("Rcppmo_th_sample_int", args = args)
  set.seed(use_seed)
  exp <- do.call("sample.int", args = args)-1
  expect_equal(act, exp)

  args["prob"] <- list(seq(1:100))
  args[c("n", "size", "replace")] <- c(100, 1e6, TRUE)
  set.seed(use_seed)
  act <- do.call("Rcppmo_th_sample_int", args = args)
  set.seed(use_seed)
  exp <- do.call("sample.int", args = args)-1
  expect_equal(act, exp)

  args["prob"] <- list(seq(1:100) / sum(1:100))
  args[c("n", "size", "replace")] <- c(100, 1e6, TRUE)
  set.seed(use_seed)
  act <- do.call("Rcppmo_th_sample_int", args = args)
  set.seed(use_seed)
  exp <- do.call("sample.int", args = args)-1
  expect_equal(act, exp)


  suppressWarnings(RNGkind(
    kind="Mersenne-Twister", normal.kind = "Inversion",
    sample.kind="Rounding"))

  args <- list("n" = 10, "size" = 10, "replace" = FALSE, "useHash" = FALSE)
  set.seed(use_seed)
  act <- do.call("Rcppmo_th_sample_int", args = args)
  set.seed(use_seed)
  exp <- do.call("sample.int", args = args)-1
  expect_equal(act, exp)

  args[c("n", "size", "replace")] <- c(1e2, 1e6, TRUE)
  set.seed(use_seed)
  act <- do.call("Rcppmo_th_sample_int", args = args)
  set.seed(use_seed)
  exp <- do.call("sample.int", args = args)-1
  expect_equal(act, exp)

  args["prob"] <- list(seq(1:100))
  args[c("n", "size", "replace")] <- c(100, 90, FALSE)
  set.seed(use_seed)
  act <- do.call("Rcppmo_th_sample_int", args = args)
  set.seed(use_seed)
  exp <- do.call("sample.int", args = args)-1
  expect_equal(act, exp)

  args["prob"] <- list(seq(1:100) / sum(1:100))
  args[c("n", "size", "replace")] <- c(100, 90, FALSE)
  set.seed(use_seed)
  act <- do.call("Rcppmo_th_sample_int", args = args)
  set.seed(use_seed)
  exp <- do.call("sample.int", args = args)-1
  expect_equal(act, exp)

  args["prob"] <- list(seq(1:100))
  args[c("n", "size", "replace")] <- c(100, 1e6, TRUE)
  set.seed(use_seed)
  act <- do.call("Rcppmo_th_sample_int", args = args)
  set.seed(use_seed)
  exp <- do.call("sample.int", args = args)-1
  expect_equal(act, exp)

  args["prob"] <- list(seq(1:100) / sum(1:100))
  args[c("n", "size", "replace")] <- c(100, 1e6, TRUE)
  set.seed(use_seed)
  act <- do.call("Rcppmo_th_sample_int", args = args)
  set.seed(use_seed)
  exp <- do.call("sample.int", args = args)-1
  expect_equal(act, exp)
})
