bf_exponential <- ExponentialBernsteinFunction(lambda = 1.133682)

test_that("Initialize `ExponentialBernsteinFunction`", {
    expect_s4_class(
        bf_exponential,
        class = "ExponentialBernsteinFunction"
    )

    expect_error(ExponentialBernsteinFunction(alpha = -0.5))
    expect_error(ExponentialBernsteinFunction(alpha = 0))
    expect_error(ExponentialBernsteinFunction(alpha = Inf))
    expect_error(ExponentialBernsteinFunction(alpha = -1))
    expect_error(ExponentialBernsteinFunction(alpha = c(1, 2)))
    expect_error(ExponentialBernsteinFunction(alpha = NA))
    expect_error(ExponentialBernsteinFunction(alpha = NaN))
})

cscale <- sqrt(2)
x <- seq(0, 10, by = 0.25)
actual_fn <- function(x, lambda) {
    x / (x + lambda)
}

test_that("`valueOf` calculates expected values", {
    expect_equal(
        valueOf(bf_exponential, x),
        actual_fn(x, bf_exponential@lambda)
    )

    expect_equal(
        valueOf(bf_exponential, x),
        valueOf0(bf_exponential, x)
    )

    expect_equal(
        valueOf(bf_exponential, x, cscale = cscale),
        actual_fn(cscale * x, bf_exponential@lambda)
    )

    expect_equal(
        valueOf(bf_exponential, x, cscale = cscale),
        valueOf(
            bf_exponential, x,
            cscale = cscale,
            method = "levy",
            tolerance = testthat_tolerance()
        )
    )

    expect_equal(
        valueOf(bf_exponential, x, cscale = cscale),
        valueOf(
            bf_exponential, x,
            cscale = cscale,
            method = "stieltjes",
            tolerance = testthat_tolerance()
        )
    )
})

d <- 7

test_that("`exIntensities` calculates expected values", {
    expect_equal(
        exIntensities(bf_exponential, d),
        ex_intensities_naive(
            actual_fn, d,
            lambda = bf_exponential@lambda
        )
    )

    expect_equal(
        exIntensities(bf_exponential, d, cscale = cscale),
        ex_intensities_naive(
            actual_fn, d,
            lambda = bf_exponential@lambda,
            cscale = cscale
        )
    )

    expect_equal(
        exIntensities(bf_exponential, d, cscale = cscale),
        exIntensities(
            bf_exponential, d,
            cscale = cscale,
            method = "levy",
            tolerance = testthat_tolerance()
        )
    )

    expect_equal(
        exIntensities(bf_exponential, d, cscale = cscale),
        exIntensities(
            bf_exponential, d,
            cscale = cscale,
            method = "stieltjes", tolerance = testthat_tolerance()
        )
    )
})

test_that("`exQMatrix` calculates expected values", {
    expect_equal(
        exQMatrix(bf_exponential, d),
        ex_qmatrix_naive(
            actual_fn, d,
            lambda = bf_exponential@lambda
        )
    )

    expect_equal(
        exQMatrix(bf_exponential, d, cscale = cscale),
        ex_qmatrix_naive(
            actual_fn, d,
            cscale = cscale,
            lambda = bf_exponential@lambda
        )
    )

    expect_equal(
        exQMatrix(bf_exponential, d, cscale = cscale),
        exQMatrix(
            bf_exponential, d,
            cscale = cscale,
            method = "levy", tolerance = testthat_tolerance()
        )
    )

    expect_equal(
        exQMatrix(bf_exponential, d, cscale = cscale),
        exQMatrix(
            bf_exponential, d,
            cscale = cscale,
            method = "stieltjes", tolerance = testthat_tolerance()
        )
    )
})
