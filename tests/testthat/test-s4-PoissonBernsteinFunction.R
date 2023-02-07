set.seed(1664)

bf_poisson <- testutils.rmo::fuzzy_bf(PoissonBernsteinFunction())

test_that("Initialize `PoissonBernsteinFunction`", {
    expect_s4_class(bf_poisson, "PoissonBernsteinFunction")

    expect_error(PoissonBernsteinFunction(eta = -0.2))
    expect_error(PoissonBernsteinFunction(eta = c(0.1, 0.2)))
    expect_error(PoissonBernsteinFunction(eta = Inf))
    expect_error(PoissonBernsteinFunction(eta = NA))
    expect_error(PoissonBernsteinFunction(eta = NaN))
})

cscale <- sqrt(2)
x <- seq(0, 10, by = 0.25)
actual_fn <- function(x, eta) {
    1 - exp(-eta * x)
}

test_that("`valueOf` calculates expected values", {
    expect_equal(
        valueOf(bf_poisson, x),
        actual_fn(x, bf_poisson@eta)
    )

    expect_equal(
        valueOf(bf_poisson, x),
        valueOf0(bf_poisson, x)
    )

    expect_equal(
        valueOf(bf_poisson, x, cscale = cscale),
        actual_fn(cscale * x, bf_poisson@eta)
    )
})

d <- 7

test_that("`exIntensities` calculates expected values", {
    expect_equal(
        exIntensities(bf_poisson, d),
        ex_intensities_naive(actual_fn, d, eta = bf_poisson@eta)
    )

    expect_equal(
        exIntensities(bf_poisson, d, cscale = cscale),
        ex_intensities_naive(
            actual_fn, d,
            eta = bf_poisson@eta,
            cscale = cscale
        )
    )

    expect_equal(
        exIntensities(bf_poisson, d, cscale = cscale),
        exIntensities(
            bf_poisson, d,
            cscale = cscale,
            method = "levy",
            tolerance = testthat_tolerance()
        )
    )
})

test_that("`exQMatrix` calculates expected values", {
    expect_equal(
        exQMatrix(bf_poisson, d),
        ex_qmatrix_naive(
            actual_fn, d,
            eta = bf_poisson@eta
        )
    )

    expect_equal(
        exQMatrix(bf_poisson, d, cscale = cscale),
        ex_qmatrix_naive(
            actual_fn, d,
            cscale = cscale,
            eta = bf_poisson@eta
        )
    )

    expect_equal(
        exQMatrix(bf_poisson, d, cscale = cscale),
        exQMatrix(
            bf_poisson, d,
            cscale = cscale,
            method = "levy", tolerance = testthat_tolerance()
        )
    )
})
