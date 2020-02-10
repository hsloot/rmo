context("Levy-frailty model with compound Poisson subordinator")
use_seed <- 1632L

## #### Test implementation with exp. dist. jumps for the bivariate case ####
#
# Test that the implementation of the Compound Poisson process LFM with
# exponentially distributed jumps works as expected for the bivaraite case
# and different choices of parameters.
test_that("biv. LFM-CPP implementation works as intended for exp. jumps", {
  n <- 25L # we use a default number of 25 simulations

  ## rate = 0.5, rate_killing = 0, rate_drift = 0, jump_rate = 2
  args <- list("d"=2L, "rate"=0.5, "rate_killing"=0, "rate_drift"=0,
  "rjump_name" = "rexp", "rjump_arg_list" = list("rate"=2))
  expect_equal_sampling_result("rmo_lfm_cpp", "test__rmo_lfm_cpp_bivariate_R",
    args, n, use_seed)

  ## rate = 0.5, rate_killing = 0.2, rate_drift = 0, jump_rate = 2
  args[["rate_killing"]] <- 0.2
  expect_equal_sampling_result("rmo_lfm_cpp", "test__rmo_lfm_cpp_bivariate_R",
    args, n, use_seed)

  ## rate = 0.5, rate_killing = 0.2, rate_drift = 0.1, jump_rate = 2
  args[["rate_drift"]] <- 0.1
  expect_equal_sampling_result("rmo_lfm_cpp", "test__rmo_lfm_cpp_bivariate_R",
    args, n, use_seed)
})

## #### Test implementation with det. jumps for the bivariate case  ####
#
# Test that the implementation of the Compound Poisson process LFM with
# deterministic jumps works as expected for the bivariate case and different
# choices of parameters.
test_that("biv. LFM-CPP implementation works as intended for det. jumps", {
  n <- 25L # we use a default number of 25 simulations

  ## rate = 0.5, rate_killing = 0, rate_drift = 0, jump_rate = 2
  args <- list("d"=2L, "rate"=0.5, "rate_killing"=0, "rate_drift"=0,
    "rjump_name" = "rposval", "rjump_arg_list" = list("value"=1))
  expect_equal_sampling_result("rmo_lfm_cpp", "test__rmo_lfm_cpp_bivariate_R",
    args, n, use_seed)

  ## rate = 0.5, rate_killing = 0.2, rate_drift = 0, jump_rate = 2
  args[["rate_killing"]] <- 0.2
  expect_equal_sampling_result("rmo_lfm_cpp", "test__rmo_lfm_cpp_bivariate_R",
    args, n, use_seed)

  ## rate = 0.5, rate_killing = 0.2, rate_drift = 0.1, jump_rate = 2
  args[["rate_drift"]] <- 0.1
  expect_equal_sampling_result("rmo_lfm_cpp", "test__rmo_lfm_cpp_bivariate_R",
    args, n, use_seed)
})


## #### Test implementation for the independence case ####
#
# Test that the implementation of the Compound Poisson process LFM with
# deterministic jumps works as expected for the independence case in d = 2.
test_that("LFM-CPP implementation works as indended for independence case", {
  n <- 25L

  args <- list("d"= 5L, "rate"=0, "rate_killing"=0, "rate_drift"=2,
    "rjump_name" = "rposval", "rjump_arg_list" = list("value"=1))
  expect_equal_sampling_result("rmo_lfm_cpp", "test__rmo_lfm_cpp_independence_R",
    args, n, use_seed)
})

## #### Test implementation for the comonotone case ####
#
# Test that the implementation of the Compound Poisson process LFM with
# deterministic jumps works as expected for the comonotone case in d = 2.
test_that("LFM-CPP implementation works as indended for comonotone case", {
  n <- 25L # we use a default number of 25 simulations

  args <- list("d"= 5L, "rate"=0, "rate_killing"=1, "rate_drift"=0,
    "rjump_name" = "rposval", "rjump_arg_list" = list("value"=1))
  expect_equal_sampling_result("rmo_lfm_cpp", "test__rmo_lfm_cpp_comonotone_R",
    args, n, use_seed)
})

## TODO: Implement test with the original implementation in `R`
