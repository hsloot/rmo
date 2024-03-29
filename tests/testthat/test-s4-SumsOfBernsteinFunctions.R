bf_sum_of_bfs <- SumOfBernsteinFunctions(
    first = AlphaStableBernsteinFunction(alpha = 0.2497179),
    second = ExponentialBernsteinFunction(lambda = 0.6492033)
)

test_that("Initialize `SumOfBernsteinFunctions`", {
    expect_s4_class(
        bf_sum_of_bfs,
        class = "SumOfBernsteinFunctions"
    )
})

cscale <- sqrt(2)
x <- seq(0, 10, by = 0.25)
actual_fn <- function(x, alpha, lambda) {
    x^alpha + 1 / (1 + lambda / x)
}

test_that("`valueOf` calculates expected values", {
    expect_equal(
        valueOf(bf_sum_of_bfs, x),
        actual_fn(
            x,
            alpha = bf_sum_of_bfs@first@alpha,
            lambda = bf_sum_of_bfs@second@lambda
        )
    )

    expect_equal(
        valueOf(bf_sum_of_bfs, x),
        valueOf0(bf_sum_of_bfs, x)
    )

    expect_equal(
        valueOf(bf_sum_of_bfs, x, cscale = cscale),
        actual_fn(
            cscale * x,
            alpha = bf_sum_of_bfs@first@alpha,
            lambda = bf_sum_of_bfs@second@lambda
        )
    )
})

d <- 7

test_that("`exIntensities` calculates expected values", {
    expect_equal(
        exIntensities(bf_sum_of_bfs, d),
        ex_intensities_naive(
            actual_fn, d,
            alpha = bf_sum_of_bfs@first@alpha,
            lambda = bf_sum_of_bfs@second@lambda
        )
    )

    expect_equal(
        exIntensities(bf_sum_of_bfs, d, cscale = cscale),
        ex_intensities_naive(
            actual_fn, d,
            alpha = bf_sum_of_bfs@first@alpha,
            lambda = bf_sum_of_bfs@second@lambda,
            cscale = cscale
        )
    )

    expect_equal(
        exIntensities(bf_sum_of_bfs, d, cscale = cscale),
        exIntensities(
            bf_sum_of_bfs, d,
            cscale = cscale,
            method = "levy",
            tolerance = testthat_tolerance()
        )
    )

    expect_equal(
        exIntensities(bf_sum_of_bfs, d, cscale = cscale),
        exIntensities(
            bf_sum_of_bfs, d,
            cscale = cscale,
            method = "stieltjes", tolerance = testthat_tolerance()
        )
    )
})

test_that("`exQMatrix` calculates expected values", {
    expect_equal(
        exQMatrix(bf_sum_of_bfs, d),
        ex_qmatrix_naive(
            actual_fn, d,
            alpha = bf_sum_of_bfs@first@alpha,
            lambda = bf_sum_of_bfs@second@lambda
        )
    )

    expect_equal(
        exQMatrix(bf_sum_of_bfs, d, cscale = cscale),
        ex_qmatrix_naive(
            actual_fn, d,
            alpha = bf_sum_of_bfs@first@alpha,
            lambda = bf_sum_of_bfs@second@lambda,
            cscale = cscale
        )
    )

    expect_equal(
        exQMatrix(bf_sum_of_bfs, d, cscale = cscale),
        exQMatrix(
            bf_sum_of_bfs, d,
            cscale = cscale,
            method = "levy", tolerance = testthat_tolerance()
        )
    )

    expect_equal(
        exQMatrix(bf_sum_of_bfs, d, cscale = cscale),
        exQMatrix(
            bf_sum_of_bfs, d,
            cscale = cscale,
            method = "stieltjes", tolerance = testthat_tolerance()
        )
    )
})
