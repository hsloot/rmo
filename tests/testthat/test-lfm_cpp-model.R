use_seed <- 1632L
n <- 100L


test_that("LFM-CPP implementation works as indended for independence case", {
  mockery::stub(rmo_lfm_cpp, "Rcpp__rmo_lfm_cpp", rtest__rmo_lfm_cpp)

  d <- 7L
  args <- list(
    "d"= d,
    "rate"=0, "rate_killing"=0, "rate_drift"=2,
    "rjump_name" = "rposval", "rjump_arg_list" = list("value"=1)
  )
  expect_equal_rn_generation(
    rmo_lfm_cpp, testutils.rmo::rmo_lfm_cpp_independence,
    args, n, use_seed)
})


test_that("LFM-CPP implementation works as indended for comonotone case", {
  mockery::stub(rmo_lfm_cpp, "Rcpp__rmo_lfm_cpp", rtest__rmo_lfm_cpp)

  d <- 7L
  args <- list(
    "d"= d,
    "rate"=0, "rate_killing"=1, "rate_drift"=0,
    "rjump_name" = "rposval", "rjump_arg_list" = list("value"=1)
  )
  expect_equal_rn_generation(
    rmo_lfm_cpp, testutils.rmo::rmo_lfm_cpp_comonotone,
    args, n, use_seed)
})


test_that("LFM-CPP implementation works as intended for exp. jumps", {
  mockery::stub(rmo_lfm_cpp, "Rcpp__rmo_lfm_cpp", rtest__rmo_lfm_cpp)

  d <- 7L
  ## rate = 0.5, rate_killing = 0, rate_drift = 0, jump_rate = 2
  args <- list(
    "d"=d,
    "rate"=0.5, "rate_killing"=0, "rate_drift"=0,
    "rjump_name" = "rexp", "rjump_arg_list" = list("rate"=2)
  )
  expect_equal_rn_generation(
    rmo_lfm_cpp, testutils.rmo::rmo_lfm_cpp_naive,
    args, n, use_seed)

  ## rate = 0.5, rate_killing = 0.2, rate_drift = 0, jump_rate = 2
  args[["rate_killing"]] <- 0.2
  expect_equal_rn_generation(
    rmo_lfm_cpp, testutils.rmo::rmo_lfm_cpp_naive,
    args, n, use_seed)

  ## rate = 0.5, rate_killing = 0.2, rate_drift = 0.1, jump_rate = 2
  args[["rate_drift"]] <- 0.1
  expect_equal_rn_generation(
    rmo_lfm_cpp, testutils.rmo::rmo_lfm_cpp_naive,
    args, n, use_seed)
})


test_that("LFM-CPP implementation works as intended for det. jumps", {
  mockery::stub(rmo_lfm_cpp, "Rcpp__rmo_lfm_cpp", rtest__rmo_lfm_cpp)

  d <- 7L
  ## rate = 0.5, rate_killing = 0, rate_drift = 0, jump_rate = 2
  args <- list(
    "d"=d,
    "rate"=0.5, "rate_killing"=0, "rate_drift"=0,
    "rjump_name" = "rposval", "rjump_arg_list" = list("value"=1)
  )
  expect_equal_rn_generation(
    rmo_lfm_cpp, testutils.rmo::rmo_lfm_cpp_naive,
    args, n, use_seed)

  ## rate = 0.5, rate_killing = 0.2, rate_drift = 0, jump_rate = 2
  args[["rate_killing"]] <- 0.2
  expect_equal_rn_generation(
    rmo_lfm_cpp, testutils.rmo::rmo_lfm_cpp_naive,
    args, n, use_seed)

  ## rate = 0.5, rate_killing = 0.2, rate_drift = 0.1, jump_rate = 2
  args[["rate_drift"]] <- 0.1
  expect_equal_rn_generation(
    rmo_lfm_cpp, testutils.rmo::rmo_lfm_cpp_naive,
    args, n, use_seed)
})

## TODO: add KS unit test
