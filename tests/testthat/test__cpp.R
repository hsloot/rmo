context("Sample compound Poisson process")
use_seed <- 1632L

## #### Test implementation with exp. dist. jumps for the bivariate case ####
#
# Test that the implementation of the Compound Poisson process LFM with
# exponentially distributed jumps works as expected for the bivariate case
# and different choices of parameters.
test_that("biv. CPP implementation works as intended for exp. jumps", {
  set.seed(use_seed-543L)
  d <- 2L
  barrier_values <- rexp(2L)
  set.seed(use_seed)

  ## rate = 0.5, rate_killing = 0, rate_drift = 0, jump_rate = 2
  args <- list("rate"=0.5, "rate_killing"=0, "rate_drift"=0,
    "rjump" = match.fun(stats::rexp), "rjump_arg_list" = list("rate"=2),
    "barrier_values"=barrier_values)
  expect_equal_sampling_result("sample_cpp", "test__sample_cpp_R",
    args, use_seed=use_seed)

  ## rate = 0.5, rate_killing = 0.2, rate_drift = 0, jump_rate = 2
  args[["rate_killing"]] <- 0.2
  expect_equal_sampling_result("sample_cpp", "test__sample_cpp_R",
    args, use_seed=use_seed)

  ## rate = 0.5, rate_killing = 0.2, rate_drift = 0.1, jump_rate = 2
  args[["rate_drift"]] <- 0.1
  expect_equal_sampling_result("sample_cpp", "test__sample_cpp_R",
    args, use_seed=use_seed)
})


## #### Test implementation with det. jumps for the bivariate case ####
#
# Test that the implementation of the Compound Poisson process with
# deterministic jumps works as expected for bivariate case and different
# choices of parameters.
test_that("biv. CPP implementation works as intended for det. jumps", {
  set.seed(use_seed-543L)
  d <- 2L
  barrier_values <- rexp(2L)
  set.seed(1623L)

  ## rate = 0.5, rate_killing = 0, rate_drift = 0, jump_rate = 2
  args <- list("rate"=0.5, "rate_killing"=0, "rate_drift"=0,
    "rjump" = match.fun(rposval), "rjump_arg_list" = list("value"=1),
    "barrier_values"=barrier_values)
  expect_equal_sampling_result("sample_cpp", "test__sample_cpp_R",
    args, use_seed=use_seed)

  ## rate = 0.5, rate_killing = 0.2, rate_drift = 0, jump_rate = 2
  args[["rate_killing"]] <- 0.2
  expect_equal_sampling_result("sample_cpp", "test__sample_cpp_R",
    args, use_seed=use_seed)

  ## rate = 0.5, rate_killing = 0.2, rate_drift = 0.1, jump_rate = 2
  args[["rate_drift"]] <- 0.1
  expect_equal_sampling_result("sample_cpp", "test__sample_cpp_R",
    args, use_seed=use_seed)
})


## #### Test implementation for the comonotone case ####
#
# Test that the implementation of the Compound Poisson process with
# deterministic jumps works as expected for d = 2 and different
# choices of parameters.
test_that("CPP implementation works as indended for comonotone case", {
  set.seed(use_seed-543L)
  d <- 5L
  barrier_values <- rexp(2L)
  set.seed(1623L)

  args <- list("rate"=0, "rate_killing"=1, "rate_drift"=0,
    "rjump" = match.fun(rposval), "rjump_arg_list" = list("value"=1),
    "barrier_values"=barrier_values)
  expect_equal_sampling_result("sample_cpp", "test__sample_cpp_R",
    args, use_seed=use_seed)
})


## TODO: Implement tests for the independence case.
