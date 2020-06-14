context("Sample compound Poisson process")
use_seed <- 1632L

test_that("CPP implementation works as intended for exp. jumps", {
  set.seed(use_seed-543L)
  d <- 5L
  barrier_values <- rexp(d)
  set.seed(use_seed)

  ## rate = 0.5, rate_killing = 0, rate_drift = 0, jump_rate = 2
  args <- list(
    "rate"=0.5, "rate_killing"=0, "rate_drift"=0,
    "rjump_name" = "rexp", "rjump_arg_list" = list("rate"=2),
    "barrier_values"=barrier_values)
  expect_equal_rn_generation(
    "sample_cpp", "test__sample_cpp",
    args, use_seed=use_seed)

  ## rate = 0.5, rate_killing = 0.2, rate_drift = 0, jump_rate = 2
  args[["rate_killing"]] <- 0.2
  expect_equal_rn_generation(
    "sample_cpp", "test__sample_cpp",
    args, use_seed=use_seed)

  ## rate = 0.5, rate_killing = 0.2, rate_drift = 0.1, jump_rate = 2
  args[["rate_drift"]] <- 0.1
  expect_equal_rn_generation(
    "sample_cpp", "test__sample_cpp",
    args, use_seed=use_seed)
})


## #### Test implementation with det. jumps for the bivariate case ####

test_that("CPP implementation works as intended for det. jumps", {
  set.seed(use_seed-543L)
  d <- 5L
  barrier_values <- rexp(d)
  set.seed(1623L)

  ## rate = 0.5, rate_killing = 0, rate_drift = 0, jump_rate = 2
  args <- list(
    "rate"=0.5, "rate_killing"=0, "rate_drift"=0,
    "rjump_name" = "rposval", "rjump_arg_list" = list("value"=1),
    "barrier_values"=barrier_values)
  expect_equal_rn_generation(
    "sample_cpp", "test__sample_cpp",
    args, use_seed=use_seed)

  ## rate = 0.5, rate_killing = 0.2, rate_drift = 0, jump_rate = 2
  args[["rate_killing"]] <- 0.2
  expect_equal_rn_generation(
    "sample_cpp", "test__sample_cpp",
    args, use_seed=use_seed)

  ## rate = 0.5, rate_killing = 0.2, rate_drift = 0.1, jump_rate = 2
  args[["rate_drift"]] <- 0.1
  expect_equal_rn_generation(
    "sample_cpp", "test__sample_cpp",
    args, use_seed=use_seed)
})


## #### Test implementation for the comonotone case ####

test_that("CPP implementation works as indended for comonotone case", {
  set.seed(use_seed-543L)
  d <- 5L
  barrier_values <- rexp(2L)
  set.seed(1623L)

  args <- list(
    "rate"=0, "rate_killing"=1, "rate_drift"=0,
    "rjump_name" = "rposval", "rjump_arg_list" = list("value"=1),
    "barrier_values"=barrier_values)
  expect_equal_rn_generation(
    "sample_cpp", "test__sample_cpp",
    args, use_seed=use_seed)
})


## #### Test implementation for the independence case ####

test_that("CPP implementation works as indended for independence case", {
  set.seed(use_seed-543L)
  d <- 5L
  barrier_values <- rexp(2L)
  set.seed(1623L)

  args <- list(
    "rate"=0, "rate_killing"=0, "rate_drift"=1,
    "rjump_name" = "rposval", "rjump_arg_list" = list("value"=1),
    "barrier_values"=barrier_values)
  expect_equal_rn_generation(
    "sample_cpp", "test__sample_cpp",
    args, use_seed=use_seed)
})
