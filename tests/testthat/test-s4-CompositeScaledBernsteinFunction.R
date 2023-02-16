bf_composite_scaled_bf <- CompositeScaledBernsteinFunction(
    cscale = 0.887174,
    original = AlphaStableBernsteinFunction(alpha = 0.7281229)
)

test_that("Initialize `CompositeScaledBernsteinFunction`", {
    expect_s4_class(
        bf_composite_scaled_bf,
        class = "CompositeScaledBernsteinFunction"
    )

    expect_error(
        ScaledBernsteinFunction(
            cscale = -1,
            ConstantBernsteinFunction(constant = 1)
        )
    )
    expect_error(
        ScaledBernsteinFunction(
            cscale = Inf,
            ConstantBernsteinFunction(constant = 1)
        )
    )
    expect_error(
        ScaledBernsteinFunction(
            cscale = NA,
            ConstantBernsteinFunction(constant = 1)
        )
    )
    expect_error(
        ScaledBernsteinFunction(
            cscale = NaN,
            ConstantBernsteinFunction(constant = 1)
        )
    )
})

cscale <- sqrt(2)
x <- seq(0, 10, by = 0.25)
actual_fn <- function(x, original_cscale, alpha) {
    (original_cscale * x)^alpha
}

test_that("`valueOf` calculates expected values", {
    expect_equal(
        valueOf(bf_composite_scaled_bf, x),
        actual_fn(
            x,
            original_cscale = bf_composite_scaled_bf@cscale,
            alpha = bf_composite_scaled_bf@original@alpha
        )
    )

    expect_equal(
        valueOf(bf_composite_scaled_bf, x),
        valueOf0(bf_composite_scaled_bf, x)
    )

    expect_equal(
        valueOf(bf_composite_scaled_bf, x, cscale = cscale),
        actual_fn(
            cscale * x,
            original_cscale = bf_composite_scaled_bf@cscale,
            alpha = bf_composite_scaled_bf@original@alpha
        )
    )
})

d <- 7

test_that("`exIntensities` calculates expected values", {
    expect_equal(
        exIntensities(bf_composite_scaled_bf, d),
        ex_intensities_naive(
            actual_fn, d,
            original_cscale = bf_composite_scaled_bf@cscale,
            alpha = bf_composite_scaled_bf@original@alpha
        )
    )

    expect_equal(
        exIntensities(bf_composite_scaled_bf, d, cscale = cscale),
        ex_intensities_naive(
            actual_fn, d,
            original_cscale = bf_composite_scaled_bf@cscale,
            alpha = bf_composite_scaled_bf@original@alpha,
            cscale = cscale
        )
    )

    expect_equal(
        exIntensities(bf_composite_scaled_bf, d, cscale = cscale),
        exIntensities(
            bf_composite_scaled_bf, d,
            cscale = cscale,
            method = "levy",
            tolerance = testthat_tolerance()
        )
    )

    expect_equal(
        exIntensities(bf_composite_scaled_bf, d, cscale = cscale),
        exIntensities(
            bf_composite_scaled_bf, d,
            cscale = cscale,
            method = "stieltjes", tolerance = testthat_tolerance()
        )
    )
})

test_that("`exQMatrix` calculates expected values", {
    expect_equal(
        exQMatrix(bf_composite_scaled_bf, d),
        ex_qmatrix_naive(
            actual_fn, d,
            original_cscale = bf_composite_scaled_bf@cscale,
            alpha = bf_composite_scaled_bf@original@alpha
        )
    )

    expect_equal(
        exQMatrix(bf_composite_scaled_bf, d, cscale = cscale),
        ex_qmatrix_naive(
            actual_fn, d,
            original_cscale = bf_composite_scaled_bf@cscale,
            alpha = bf_composite_scaled_bf@original@alpha,
            cscale = cscale
        )
    )

    expect_equal(
        exQMatrix(bf_composite_scaled_bf, d, cscale = cscale),
        exQMatrix(
            bf_composite_scaled_bf, d,
            cscale = cscale,
            method = "levy", tolerance = testthat_tolerance()
        )
    )

    expect_equal(
        exQMatrix(bf_composite_scaled_bf, d, cscale = cscale),
        exQMatrix(
            bf_composite_scaled_bf, d,
            cscale = cscale,
            method = "stieltjes", tolerance = testthat_tolerance()
        )
    )
})
