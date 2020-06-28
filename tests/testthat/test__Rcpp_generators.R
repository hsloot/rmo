context("Rcpp generators")
use_seed <- 1632L
n <- 1e5L

test_that("RExpGenerator works as expected", {
  args <- list("rate" = 0.5)
  expect_equal_rn_generation(
    "rexp", "mo_internal__rexp",
    args, n, use_seed)
  args <- list("rate" = 2)
  expect_equal_rn_generation(
    "rexp", "mo_internal__rexp",
    args, n, use_seed)
  rexp_rate_is_zero <- function(n, rate) {
    stopifnot(rate == 0.)
    rep(Inf, times=n)
  }
  args <- list("rate" = 0)
  expect_equal_rn_generation(
    "rexp_rate_is_zero", "mo_internal__rexp",
    args, n, use_seed)
  rexp_rate_is_inf <- function(n, rate) {
    stopifnot(rate == Inf)
    rep(0., times=n)
  }
  args <- list("rate" = Inf)
  expect_equal_rn_generation(
    "rexp_rate_is_inf", "mo_internal__rexp",
    args, n, use_seed)
})

test_that("FixedDblGenerator works as expected", {
  args <- list("value" = 1)
  expect_equal_rn_generation(
    "rposval", "mo_internal__fixeddbl",
    args, n, use_seed)
})

test_that("CountReplaceGenerator works as expected", {
  sample_count_replace_base_0 <- function(n, d = length(probabilities), probabilities = NULL) {
    sapply(
      1:n,
      function(x) {
        sample.int(
          n=length(probabilities), size=1,
          prob = probabilities, replace=FALSE) - 1
      }
    )
  }
  probabilities <- c(8, 7, 3, 10, 6, 1, 2, 9, 5, 4)
  args <- list("d" = length(probabilities), "probabilities" = probabilities)
  expect_equal_rn_generation(
    "mo_internal__count_replace", "sample_count_replace_base_0",
    args, n, use_seed)

  probabilities <- probabilities / sum(probabilities)
  args <- list("d" = length(probabilities), "probabilities" = probabilities)
  expect_equal_rn_generation(
    "mo_internal__count_replace", "sample_count_replace_base_0",
    args, n, use_seed)

  probabilities <- rep(1, 10) / 10
  args <- list("d" = length(probabilities), "probabilities" = probabilities)
  expect_equal_rn_generation(
    "mo_internal__count_replace", "sample_count_replace_base_0",
    args, n, use_seed)

  probabilities <- c(10, 20, 0.1)
  args <- list("d" = length(probabilities), "probabilities" = probabilities)
  expect_equal_rn_generation(
    "mo_internal__count_replace", "sample_count_replace_base_0",
    args, n, use_seed)
})

test_that("CountNoReplaceWalker works as expected", {
  sample_count_noreplace_base_0 <- function(n, d = length(probabilities), probabilities = NULL) {
    -1+sample.int(
      n=length(probabilities), size=n,
      prob = probabilities, replace=FALSE)
  }
  probabilities <- c(8, 7, 3, 10, 6, 1, 2, 9, 5, 4)
  n <- length(probabilities)
  args <- list("d" = length(probabilities), "probabilities" = probabilities)
  expect_equal_rn_generation(
    "mo_internal__count_noreplace", "sample_count_noreplace_base_0",
    args, n, use_seed)

  probabilities <- probabilities / sum(probabilities)
  n <- length(probabilities)
  args <- list("d" = length(probabilities), "probabilities" = probabilities)
  expect_equal_rn_generation(
    "mo_internal__count_noreplace", "sample_count_noreplace_base_0",
    args, n, use_seed)

  probabilities <- rep(1, 10) / 10
  n <- length(probabilities)
  args <- list("d" = length(probabilities), "probabilities" = probabilities)
  expect_equal_rn_generation(
    "mo_internal__count_noreplace", "sample_count_noreplace_base_0",
    args, n, use_seed)

  probabilities <- c(10, 20, 0.1)
  n <- length(probabilities)
  args <- list("d" = length(probabilities), "probabilities" = probabilities)
  expect_equal_rn_generation(
    "mo_internal__count_noreplace", "sample_count_noreplace_base_0",
    args, n, use_seed)
})


test_that("sample.int reimplementation works as expected", {
  sample_int_base_0 <- function(
      n=length(probabilities), size, prob = NULL, replace=FALSE) {
    sample.int(n=n, size=size, prob = prob, replace=replace) - 1
  }

  args <- list("n" = 10, "size" = 10, "replace" = FALSE)
  expect_equal_rn_generation(
    "mo_internal__sample_int", "sample_int_base_0",
    arg_list=args, use_seed=1623L)

  args[c("n", "size", "replace")] <- c(1e2, 1e6, TRUE)
  expect_equal_rn_generation(
    "mo_internal__sample_int", "sample_int_base_0",
    arg_list=args, use_seed=1623L)

  args["prob"] <- list(seq(1:100))
  args[c("n", "size", "replace")] <- c(100, 90, FALSE)
  expect_equal_rn_generation(
    "mo_internal__sample_int", "sample_int_base_0",
    arg_list=args, use_seed=1623L)

  args["prob"] <- list(seq(1:100) / sum(1:100))
  args[c("n", "size", "replace")] <- c(100, 90, FALSE)
  expect_equal_rn_generation(
    "mo_internal__sample_int", "sample_int_base_0",
    arg_list=args, use_seed=1623L)

  args["prob"] <- list(seq(1:100))
  args[c("n", "size", "replace")] <- c(100, 1e6, TRUE)
  expect_equal_rn_generation(
    "mo_internal__sample_int", "sample_int_base_0",
    arg_list=args, use_seed=1623L)

  args["prob"] <- list(seq(1:100) / sum(1:100))
  args[c("n", "size", "replace")] <- c(100, 1e6, TRUE)
  expect_equal_rn_generation(
    "mo_internal__sample_int", "sample_int_base_0",
    arg_list=args, use_seed=1623L)

  args <- list("n" = 10, "size" = 10, "replace" = FALSE)
  set.seed(use_seed)
  expect_equal_rn_generation(
    "mo_internal__sample_int", "sample_int_base_0",
    arg_list=args, use_seed=1623L)

  args[c("n", "size", "replace")] <- c(1e2, 1e6, TRUE)
  set.seed(use_seed)
  expect_equal_rn_generation(
    "mo_internal__sample_int", "sample_int_base_0",
    arg_list=args, use_seed=1623L)

  args["prob"] <- list(seq(1:100))
  args[c("n", "size", "replace")] <- c(100, 90, FALSE)
  expect_equal_rn_generation(
    "mo_internal__sample_int", "sample_int_base_0",
    arg_list=args, use_seed=1623L)

  args["prob"] <- list(seq(1:100) / sum(1:100))
  args[c("n", "size", "replace")] <- c(100, 90, FALSE)
  expect_equal_rn_generation(
    "mo_internal__sample_int", "sample_int_base_0",
    arg_list=args, use_seed=1623L)

  args["prob"] <- list(seq(1:100))
  args[c("n", "size", "replace")] <- c(100, 1e6, TRUE)
  expect_equal_rn_generation(
    "mo_internal__sample_int", "sample_int_base_0",
    arg_list=args, use_seed=1623L)

  args["prob"] <- list(seq(1:100) / sum(1:100))
  args[c("n", "size", "replace")] <- c(100, 1e6, TRUE)
  expect_equal_rn_generation(
    "mo_internal__sample_int", "sample_int_base_0",
    arg_list=args, use_seed=1623L)
})
