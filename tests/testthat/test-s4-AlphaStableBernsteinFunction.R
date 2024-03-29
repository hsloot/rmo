bf_alpha_stable <- AlphaStableBernsteinFunction(alpha = 0.6132243)

test_that("Initialize `AlphaStableBernsteinFunction`", {
    expect_s4_class(bf_alpha_stable, class = "AlphaStableBernsteinFunction")

    expect_error(AlphaStableBernsteinFunction(alpha = -0.5))
    expect_error(AlphaStableBernsteinFunction(alpha = 0))
    expect_error(AlphaStableBernsteinFunction(alpha = 1))
    expect_error(AlphaStableBernsteinFunction(alpha = 1.5))
    expect_error(AlphaStableBernsteinFunction(alpha = -1))
    expect_error(AlphaStableBernsteinFunction(alpha = c(1, 2)))
    expect_error(AlphaStableBernsteinFunction(alpha = NA))
    expect_error(AlphaStableBernsteinFunction(alpha = NaN))
})

cscale <- sqrt(2)
x <- seq(0, 10, by = 0.25)
actual_fn <- function(x, alpha) {
    x^alpha
}

test_that("`valueOf` calculates expected values", {
    expect_equal(
        valueOf(bf_alpha_stable, x),
        actual_fn(x, bf_alpha_stable@alpha)
    )

    expect_equal(
        valueOf(bf_alpha_stable, x),
        valueOf0(bf_alpha_stable, x)
    )

    expect_equal(
        valueOf(bf_alpha_stable, x, cscale = cscale),
        actual_fn(cscale * x, bf_alpha_stable@alpha)
    )

    expect_error(
        valueOf(
            AlphaStableBernsteinFunction(log2(2 - 1e-4)),
            x,
            difference_order = 1L
        ), NA
    )
    expect_error(
        valueOf(
            AlphaStableBernsteinFunction(log2(2 - (1 - 1e-4))),
            x,
            difference_order = 1L
        ), NA
    )
})

d <- 7

test_that("`exIntensities` calculates expected values", {
    expect_equal(
        exIntensities(bf_alpha_stable, d),
        ex_intensities_naive(actual_fn, d, alpha = bf_alpha_stable@alpha)
    )

    expect_equal(
        exIntensities(bf_alpha_stable, d, cscale = cscale),
        ex_intensities_naive(
            actual_fn, d,
            alpha = bf_alpha_stable@alpha,
            cscale = cscale
        )
    )

    expect_equal(
        exIntensities(bf_alpha_stable, d, cscale = cscale),
        exIntensities(
            bf_alpha_stable, d,
            cscale = cscale,
            method = "levy",
            tolerance = testthat_tolerance()
        )
    )

    expect_equal(
        exIntensities(bf_alpha_stable, d, cscale = cscale),
        exIntensities(
            bf_alpha_stable, d,
            cscale = cscale,
            method = "stieltjes", tolerance = testthat_tolerance()
        )
    )
})

test_that("`exQMatrix` calculates expected values", {
    expect_equal(
        exQMatrix(bf_alpha_stable, d),
        ex_qmatrix_naive(
            actual_fn, d,
            alpha = bf_alpha_stable@alpha
        )
    )

    expect_equal(
        exQMatrix(bf_alpha_stable, d, cscale = cscale),
        ex_qmatrix_naive(
            actual_fn, d,
            cscale = cscale,
            alpha = bf_alpha_stable@alpha
        )
    )

    expect_equal(
        exQMatrix(bf_alpha_stable, d, cscale = cscale),
        exQMatrix(
            bf_alpha_stable, d,
            cscale = cscale,
            method = "levy", tolerance = testthat_tolerance()
        )
    )

    expect_equal(
        exQMatrix(bf_alpha_stable, d, cscale = cscale),
        exQMatrix(
            bf_alpha_stable, d,
            cscale = cscale,
            method = "stieltjes", tolerance = testthat_tolerance()
        )
    )
})
