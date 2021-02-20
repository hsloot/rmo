use_seed <- 1632
n <- 1e2


test_that("LFM-CPP implementation works as indended for independence case", {
  mockery::stub(rextmo_lfm, "Rcpp__rextmo_lfm", rtest__rextmo_lfm)

  d <- 7
  args <- list(
    "d" = d,
    "rate" = 0, "rate_killing" = 0, "rate_drift" = 2,
    "rjump_name" = "rposval",
    "rjump_arg_list" = list("value" = 0)
  )
  expect_equal_rn_generation(
    rextmo_lfm, testutils.rmo::rextmo_lfm_independence,
    args, n, use_seed)
})


test_that("LFM-CPP implementation works as indended for comonotone case", {
  mockery::stub(rextmo_lfm, "Rcpp__rextmo_lfm", rtest__rextmo_lfm)

  d <- 7
  args <- list(
    "d" = d,
    "rate" = 0, "rate_killing" = 1, "rate_drift" = 0,
    "rjump_name" = "rposval",
    "rjump_arg_list" = list("value" = 0)
  )
  expect_equal_rn_generation(
    rextmo_lfm, testutils.rmo::rextmo_lfm_comonotone,
    args, n, use_seed)
})


test_that("LFM-CPP implementation works as intended for exp. jumps", {
  mockery::stub(rextmo_lfm, "Rcpp__rextmo_lfm", rtest__rextmo_lfm)

  d <- 7

  ## no killing, no drift
  args <- list(
    "d" = d,
    "rate" = 0.5, "rate_killing" = 0, "rate_drift" = 0,
    "rjump_name" = "rexp",
    "rjump_arg_list" = list("rate" = 2)
  )
  expect_equal_rn_generation(
    rextmo_lfm, testutils.rmo::rextmo_lfm_naive,
    args, n, use_seed)

  ## with killing, no drift
  args <- list(
    "d" = d,
    "rate" = 0.5, "rate_killing" = 0.2, "rate_drift" = 0,
    "rjump_name" = "rexp",
    "rjump_arg_list" = list("rate" = 2)
  )
  expect_equal_rn_generation(
    rextmo_lfm, testutils.rmo::rextmo_lfm_naive,
    args, n, use_seed)

  ## no killing, with drift
  args <- list(
    "d" = d,
    "rate" = 0.5, "rate_killing" = 0, "rate_drift" = 0.1,
    "rjump_name" = "rexp",
    "rjump_arg_list" = list("rate" = 2)
  )
  expect_equal_rn_generation(
    rextmo_lfm, testutils.rmo::rextmo_lfm_naive,
    args, n, use_seed)

  ## with killing, with drift
  args <- list(
    "d" = d,
    "rate" = 0.5, "rate_killing" = 0.2, "rate_drift" = 0.1,
    "rjump_name" = "rexp",
    "rjump_arg_list" = list("rate" = 2)
  )
  expect_equal_rn_generation(
    rextmo_lfm, testutils.rmo::rextmo_lfm_naive,
    args, n, use_seed)
})


test_that("LFM-CPP implementation works as intended for det. jumps", {
  mockery::stub(rextmo_lfm, "Rcpp__rextmo_lfm", rtest__rextmo_lfm)

  d <- 7L

  ## no killing, no drift
  args <- list(
    "d" = d,
    "rate" = 0.5, "rate_killing" = 0, "rate_drift" = 0,
    "rjump_name" = "rposval",
    "rjump_arg_list" = list("value" = 1)
  )
  expect_equal_rn_generation(
    rextmo_lfm, testutils.rmo::rextmo_lfm_naive,
    args, n, use_seed)

  ## with killing, no drift
  args <- list(
    "d" = d,
    "rate" = 0.5, "rate_killing" = 0.2, "rate_drift" = 0,
    "rjump_name" = "rposval",
    "rjump_arg_list" = list("value" = 1)
  )
  expect_equal_rn_generation(
    rextmo_lfm, testutils.rmo::rextmo_lfm_naive,
    args, n, use_seed)

  ## no killing, with drift
  args <- list(
    "d" = d,
    "rate" = 0.5, "rate_killing" = 0, "rate_drift" = 0.1,
    "rjump_name" = "rposval",
    "rjump_arg_list" = list("value" = 1)
  )
  expect_equal_rn_generation(
    rextmo_lfm, testutils.rmo::rextmo_lfm_naive,
    args, n, use_seed)

  ## with killing, with drift
  args <- list(
    "d" = d,
    "rate" = 0.5, "rate_killing" = 0.2, "rate_drift" = 0.1,
    "rjump_name" = "rposval",
    "rjump_arg_list" = list("value" = 1)
  )
  expect_equal_rn_generation(
    rextmo_lfm, testutils.rmo::rextmo_lfm_naive,
    args, n, use_seed)
})


test_that("LFM-CPP implementation works as intended for pareto jumps", {
  mockery::stub(rextmo_lfm, "Rcpp__rextmo_lfm", rtest__rextmo_lfm)

  d <- 7L

  ## no killing, no drift
  args <- list(
    "d" = d,
    "rate" = 0.5, "rate_killing" = 0, "rate_drift" = 0,
    "rjump_name" = "rpareto",
    "rjump_arg_list" = list("alpha" = 0.4, "x0" = 1e-4)
  )
  expect_equal_rn_generation(
    rextmo_lfm, testutils.rmo::rextmo_lfm_naive,
    args, n, use_seed)

  ## with killing, no drift
  args <- list(
    "d" = d,
    "rate" = 0.5, "rate_killing" = 0.2, "rate_drift" = 0,
    "rjump_name" = "rpareto",
    "rjump_arg_list" = list("alpha" = 0.4, "x0" = 1e-4)
  )
  expect_equal_rn_generation(
    rextmo_lfm, testutils.rmo::rextmo_lfm_naive,
    args, n, use_seed)

  ## no killing, with drift
  args <- list(
    "d" = d,
    "rate" = 0.5, "rate_killing" = 0, "rate_drift" = 0.1,
    "rjump_name" = "rpareto",
    "rjump_arg_list" = list("alpha" = 0.4, "x0" = 1e-4)
  )
  expect_equal_rn_generation(
    rextmo_lfm, testutils.rmo::rextmo_lfm_naive,
    args, n, use_seed)

  ## with killing, with drift
  args <- list(
    "d" = d,
    "rate" = 0.5, "rate_killing" = 0.2, "rate_drift" = 0.1,
    "rjump_name" = "rpareto",
    "rjump_arg_list" = list("alpha" = 0.4, "x0" = 1e-4)
  )
  expect_equal_rn_generation(
    rextmo_lfm, testutils.rmo::rextmo_lfm_naive,
    args, n, use_seed)
})
