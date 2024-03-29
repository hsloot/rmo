use_seed <- 1632
n <- 1e2

rextmo_lfm <- function(
        n,
        d,
        rate,
        rate_killing,
        rate_drift,
        rjump_name,
        rjump_arg_list) {
    mockery::stub(rextmo_lfm, "Rcpp__rextmo_lfm", rtest__rextmo_lfm)
    if (isTRUE(rate == 0) ||
            (isTRUE(rjump_name == "rposval") &&
                 isTRUE(rjump_arg_list$value == 0))) {
        rpextmo(
            n, d,
            a = rate_killing, b = rate_drift,
            family = "Armageddon", method = "LFM"
        )
    } else if (isTRUE("rposval" == rjump_name)) {
        rpextmo(
            n, d,
            a = rate_killing, b = rate_drift, gamma = rate,
            eta = rjump_arg_list$value,
            family = "Poisson", method = "LFM"
        )
    } else if (isTRUE("rpareto" == rjump_name)) {
        rpextmo(
            n, d,
            a = rate_killing, b = rate_drift, gamma = rate,
            eta = c(rjump_arg_list$alpha, rjump_arg_list$x0),
            family = "Pareto", method = "LFM"
        )
    } else if (isTRUE("rexp" == rjump_name)) {
        rpextmo(
            n, d,
            a = rate_killing, b = rate_drift, gamma = rate,
            eta = rjump_arg_list$rate,
            family = "Exponential", method = "LFM"
        )
    } else {
        stop(sprintf("Jump distribution %s not implemented", rjump_name))
    }
}

test_that("LFM-CPP implementation works as indended for independence case", {
    d <- 7
    args <- list(
        "d" = d,
        "rate" = 0, "rate_killing" = 0, "rate_drift" = 2,
        "rjump_name" = "rposval",
        "rjump_arg_list" = list("value" = 0)
    )
    expect_equal_rn_generation(
        rextmo_lfm, testutils.rmo::rextmo_lfm_independence,
        args, n, use_seed
    )
})


test_that("LFM-CPP implementation works as indended for comonotone case", {
    d <- 7
    args <- list(
        "d" = d,
        "rate" = 0, "rate_killing" = 1, "rate_drift" = 0,
        "rjump_name" = "rposval",
        "rjump_arg_list" = list("value" = 0)
    )
    expect_equal_rn_generation(
        rextmo_lfm, testutils.rmo::rextmo_lfm_comonotone,
        args, n, use_seed
    )
})


test_that("LFM-CPP implementation works as intended for exp. jumps", {
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
        args, n, use_seed
    )

    ## with killing, no drift
    args <- list(
        "d" = d,
        "rate" = 0.5, "rate_killing" = 0.2, "rate_drift" = 0,
        "rjump_name" = "rexp",
        "rjump_arg_list" = list("rate" = 2)
    )
    expect_equal_rn_generation(
        rextmo_lfm, testutils.rmo::rextmo_lfm_naive,
        args, n, use_seed
    )

    ## no killing, with drift
    args <- list(
        "d" = d,
        "rate" = 0.5, "rate_killing" = 0, "rate_drift" = 0.1,
        "rjump_name" = "rexp",
        "rjump_arg_list" = list("rate" = 2)
    )
    expect_equal_rn_generation(
        rextmo_lfm, testutils.rmo::rextmo_lfm_naive,
        args, n, use_seed
    )

    ## with killing, with drift
    args <- list(
        "d" = d,
        "rate" = 0.5, "rate_killing" = 0.2, "rate_drift" = 0.1,
        "rjump_name" = "rexp",
        "rjump_arg_list" = list("rate" = 2)
    )
    expect_equal_rn_generation(
        rextmo_lfm, testutils.rmo::rextmo_lfm_naive,
        args, n, use_seed
    )
})


test_that("LFM-CPP implementation works as intended for det. jumps", {
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
        args, n, use_seed
    )

    ## with killing, no drift
    args <- list(
        "d" = d,
        "rate" = 0.5, "rate_killing" = 0.2, "rate_drift" = 0,
        "rjump_name" = "rposval",
        "rjump_arg_list" = list("value" = 1)
    )
    expect_equal_rn_generation(
        rextmo_lfm, testutils.rmo::rextmo_lfm_naive,
        args, n, use_seed
    )

    ## no killing, with drift
    args <- list(
        "d" = d,
        "rate" = 0.5, "rate_killing" = 0, "rate_drift" = 0.1,
        "rjump_name" = "rposval",
        "rjump_arg_list" = list("value" = 1)
    )
    expect_equal_rn_generation(
        rextmo_lfm, testutils.rmo::rextmo_lfm_naive,
        args, n, use_seed
    )

    ## with killing, with drift
    args <- list(
        "d" = d,
        "rate" = 0.5, "rate_killing" = 0.2, "rate_drift" = 0.1,
        "rjump_name" = "rposval",
        "rjump_arg_list" = list("value" = 1)
    )
    expect_equal_rn_generation(
        rextmo_lfm, testutils.rmo::rextmo_lfm_naive,
        args, n, use_seed
    )
})


test_that("LFM-CPP implementation works as intended for pareto jumps", {
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
        args, n, use_seed
    )

    ## with killing, no drift
    args <- list(
        "d" = d,
        "rate" = 0.5, "rate_killing" = 0.2, "rate_drift" = 0,
        "rjump_name" = "rpareto",
        "rjump_arg_list" = list("alpha" = 0.4, "x0" = 1e-4)
    )
    expect_equal_rn_generation(
        rextmo_lfm, testutils.rmo::rextmo_lfm_naive,
        args, n, use_seed
    )

    ## no killing, with drift
    args <- list(
        "d" = d,
        "rate" = 0.5, "rate_killing" = 0, "rate_drift" = 0.1,
        "rjump_name" = "rpareto",
        "rjump_arg_list" = list("alpha" = 0.4, "x0" = 1e-4)
    )
    expect_equal_rn_generation(
        rextmo_lfm, testutils.rmo::rextmo_lfm_naive,
        args, n, use_seed
    )

    ## with killing, with drift
    args <- list(
        "d" = d,
        "rate" = 0.5, "rate_killing" = 0.2, "rate_drift" = 0.1,
        "rjump_name" = "rpareto",
        "rjump_arg_list" = list("alpha" = 0.4, "x0" = 1e-4)
    )
    expect_equal_rn_generation(
        rextmo_lfm, testutils.rmo::rextmo_lfm_naive,
        args, n, use_seed
    )
})

# #### Test no-sample ####

test_that("LFM implementation for n = 0", {
    n <- 0
    d <- 5
    x <- rextmo_lfm(
        n, d,
        rate = 1, rate_killing = 0, rate_drift = 0,
        rjump_name = "rexp", rjump_arg_list = list(rate = 1)
    )
    checkmate::expect_matrix(x, mode = "numeric", nrows = n, ncols = d)
})
