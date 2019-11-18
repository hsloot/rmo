context("Lévy-frailty model with compound Poisson subordinator")
use_seed <- 1632L

test_that("biv. LFM-CPP implementation works as intended for exp. jumps", {
  n <- 25L

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

test_that("biv. LFM-CPP implementation works as intended for det. jumps", {
  n <- 25L

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

test_that("LFM-CPP implementation works as indended for comonotone case", {
  n <- 25L

  args <- list("d"= 5L, "rate"=0, "rate_killing"=1, "rate_drift"=0,
    "rjump_name" = "rposval", "rjump_arg_list" = list("value"=1))
  expect_equal_sampling_result("rmo_lfm_cpp", "test__rmo_lfm_cpp_comonotone_R",
    args, n, use_seed)
})
