bf_scaled_bf <- ScaledBernsteinFunction(
    scale = 4.638729,
    original = AlphaStableBernsteinFunction(alpha = 0.2188059)
)

test_that("Initialize `ScaledBernsteinFunction`", {
    expect_s4_class(bf_scaled_bf, "ScaledBernsteinFunction")

    expect_error(
        ScaledBernsteinFunction(
            scale = -1,
            ConstantBernsteinFunction(constant = 1)
        )
    )
    expect_error(
        ScaledBernsteinFunction(
            scale = Inf,
            ConstantBernsteinFunction(constant = 1)
        )
    )
    expect_error(
        ScaledBernsteinFunction(
            scale = NA,
            ConstantBernsteinFunction(constant = 1)
        )
    )
    expect_error(
        ScaledBernsteinFunction(
            scale = NaN,
            ConstantBernsteinFunction(constant = 1)
        )
    )
})

cscale <- sqrt(2)
x <- seq(0, 10, by = 0.25)
actual_fn <- function(x, scale, alpha) {
    scale * x^alpha
}

test_that("`valueOf` calculates expected values", {
    expect_equal(
        valueOf(bf_scaled_bf, x),
        actual_fn(
            x,
            scale = bf_scaled_bf@scale, alpha = bf_scaled_bf@original@alpha
        )
    )

    expect_equal(
        valueOf(bf_scaled_bf, x),
        valueOf0(bf_scaled_bf, x)
    )

    expect_equal(
        valueOf(bf_scaled_bf, x, cscale = cscale),
        actual_fn(
            cscale * x,
            scale = bf_scaled_bf@scale,
            alpha = bf_scaled_bf@original@alpha
        )
    )
})

d <- 7

test_that("`exIntensities` calculates expected values", {
    expect_equal(
        exIntensities(bf_scaled_bf, d),
        ex_intensities_naive(
            actual_fn, d,
            scale = bf_scaled_bf@scale,
            alpha = bf_scaled_bf@original@alpha
        )
    )

    expect_equal(
        exIntensities(bf_scaled_bf, d, cscale = cscale),
        ex_intensities_naive(
            actual_fn, d,
            scale = bf_scaled_bf@scale,
            alpha = bf_scaled_bf@original@alpha,
            cscale = cscale
        )
    )

    expect_equal(
        exIntensities(bf_scaled_bf, d, cscale = cscale),
        exIntensities(
            bf_scaled_bf, d,
            cscale = cscale,
            method = "levy",
            tolerance = testthat_tolerance()
        )
    )

    expect_equal(
        exIntensities(bf_scaled_bf, d, cscale = cscale),
        exIntensities(
            bf_scaled_bf, d,
            cscale = cscale,
            method = "stieltjes", tolerance = testthat_tolerance()
        )
    )
})

test_that("`exQMatrix` calculates expected values", {
    expect_equal(
        exQMatrix(bf_scaled_bf, d),
        ex_qmatrix_naive(
            actual_fn, d,
            scale = bf_scaled_bf@scale,
            alpha = bf_scaled_bf@original@alpha
        )
    )

    expect_equal(
        exQMatrix(bf_scaled_bf, d, cscale = cscale),
        ex_qmatrix_naive(
            actual_fn, d,
            cscale = cscale,
            scale = bf_scaled_bf@scale,
            alpha = bf_scaled_bf@original@alpha
        )
    )

    expect_equal(
        exQMatrix(bf_scaled_bf, d, cscale = cscale),
        exQMatrix(
            bf_scaled_bf, d,
            cscale = cscale,
            method = "levy", tolerance = testthat_tolerance()
        )
    )

    expect_equal(
        exQMatrix(bf_scaled_bf, d, cscale = cscale),
        exQMatrix(
            bf_scaled_bf, d,
            cscale = cscale,
            method = "stieltjes", tolerance = testthat_tolerance()
        )
    )
})
