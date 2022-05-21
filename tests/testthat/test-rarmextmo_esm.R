use_seed <- 1623
n <- 1e2

rarmextmo_esm <- function(n, d, alpha, beta) {
    mockery::stub(rpextmo, "Rcpp__rarmextmo_esm", rtest__rarmextmo_esm)
    rpextmo(n, d, a = beta, b = alpha, family = "Armageddon", method = "ESM")
}

## #### Test implementation for the bivariate case ####

test_that("Armageddon shock Ext.-MO implementation works as intended for d=2", {
    ## both equal
    args <- list("d" = 2, "alpha" = 1, "beta" = 1)
    expect_equal_rn_generation(rarmextmo_esm, testutils.rmo::rarmextmo_esm_bivariate,
        args, n, use_seed)

    ## independence
    args <- list("d" = 2, "alpha" = 1, "beta" = 0)
    expect_equal_rn_generation(rarmextmo_esm, testutils.rmo::rarmextmo_esm_bivariate,
        args, n, use_seed)

    ## comonotone
    args <- list("d" = 2, "alpha" = 0, "beta" = 1)
    expect_equal_rn_generation(rarmextmo_esm, testutils.rmo::rarmextmo_esm_bivariate,
        args, n, use_seed)

    ## alpha = 0.5, beta = 0.3
    args <- list("d" = 2, "alpha" = 0.5, "beta" = 0.3)
    expect_equal_rn_generation(rarmextmo_esm, testutils.rmo::rarmextmo_esm_bivariate,
        args, n, use_seed)
})



## #### Test implementation against original `R` version ####

test_that("Armageddon shock Ext.-MO ESM implementation in Rcpp", {
    d <- 7

    args <- list("d" = d, "alpha" = 1, "beta" = 1)
    expect_equal_rn_generation(rarmextmo_esm, testutils.rmo::rarmextmo_esm_naive,
        args, n, use_seed)

    ## independence
    args <- list("d" = d, "alpha" = 1, "beta" = 0)
    expect_equal_rn_generation(rarmextmo_esm, testutils.rmo::rarmextmo_esm_naive,
        args, n, use_seed)

    ## comonotone
    args <- list("d" = d, "alpha" = 0, "beta" = 1)
    expect_equal_rn_generation(rarmextmo_esm, testutils.rmo::rarmextmo_esm_naive,
        args, n, use_seed)

    ## alpha = 0.5, beta = 0.3
    args <- list("d" = d, "alpha" = 0.5, "beta" = 0.3)
    expect_equal_rn_generation(rarmextmo_esm, testutils.rmo::rarmextmo_esm_naive,
        args, n, use_seed)
})

# #### Test no-sample ####

test_that("Arm. ESM implementation for n = 0", {
    n <- 0
    d <- 5
    x <- rarmextmo_esm(n, d, alpha = 1, beta = 1 / d)
    checkmate::expect_matrix(x, mode = "numeric", nrows = n, ncols = d)
})
