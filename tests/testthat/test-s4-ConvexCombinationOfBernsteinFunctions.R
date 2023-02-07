set.seed(1643)

bf_convex_combinations_of_bfs <- testutils.rmo::fuzzy_bf(
    ConvexCombinationOfBernsteinFunctions()
)

test_that("Initialize `ConvexCombinationsOfBernsteinFunctions`", {
    expect_s4_class(
        bf_convex_combinations_of_bfs,
        class = "ConvexCombinationOfBernsteinFunctions"
    )
})

cscale <- sqrt(2)
x <- seq(0, 10, by = 0.25)
actual_fn <- function(x, coefficients, points) {
    drop(
        t(coefficients) %*% drop(t(sapply(points, valueOf0, x = x)))
    )
}

has_levy_density <- function(bf) {
    all(
        sapply(
            bf@points,
            function(x) {
                isTRUE(inherits(x, "LevyBernsteinFunction"))
            }
        )
    )
}

has_stieltjes_density <- function(bf) {
    all(
        sapply(
            bf@points,
            function(x) {
                isTRUE(inherits(x, "CompleteBernsteinFunction"))
            }
        )
    )
}

test_that("`valueOf` calculates expected values", {
    expect_equal(
        valueOf(bf_convex_combinations_of_bfs, x),
        actual_fn(
            x,
            coefficients = bf_convex_combinations_of_bfs@coefficients,
            points = bf_convex_combinations_of_bfs@points
        )
    )

    expect_equal(
        valueOf(bf_convex_combinations_of_bfs, x),
        valueOf0(bf_convex_combinations_of_bfs, x)
    )

    expect_equal(
        valueOf(bf_convex_combinations_of_bfs, x, cscale = cscale),
        actual_fn(
            cscale * x,
            coefficients = bf_convex_combinations_of_bfs@coefficients,
            points = bf_convex_combinations_of_bfs@points
        )
    )
})

d <- 7

test_that("`exIntensities` calculates expected values", {
    expect_equal(
        exIntensities(bf_convex_combinations_of_bfs, d),
        ex_intensities_naive(
            actual_fn, d,
            coefficients = bf_convex_combinations_of_bfs@coefficients,
            points = bf_convex_combinations_of_bfs@points
        )
    )

    expect_equal(
        exIntensities(bf_convex_combinations_of_bfs, d, cscale = cscale),
        ex_intensities_naive(
            actual_fn, d,
            coefficients = bf_convex_combinations_of_bfs@coefficients,
            points = bf_convex_combinations_of_bfs@points,
            cscale = cscale
        )
    )

    skip_if_not(has_levy_density(bf_convex_combinations_of_bfs))
    expect_equal(
        exIntensities(bf_convex_combinations_of_bfs, d, cscale = cscale),
        exIntensities(
            bf_convex_combinations_of_bfs, d,
            cscale = cscale,
            method = "levy",
            tolerance = testthat_tolerance()
        )
    )

    skip_if_not(has_stieltjes_density(bf_convex_combinations_of_bfs))
    expect_equal(
        exIntensities(bf_convex_combinations_of_bfs, d, cscale = cscale),
        exIntensities(
            bf_convex_combinations_of_bfs, d,
            cscale = cscale,
            method = "stieltjes", tolerance = testthat_tolerance()
        )
    )
})

test_that("`exQMatrix` calculates expected values", {
    expect_equal(
        exQMatrix(bf_convex_combinations_of_bfs, d),
        ex_qmatrix_naive(
            actual_fn, d,
            coefficients = bf_convex_combinations_of_bfs@coefficients,
            points = bf_convex_combinations_of_bfs@points
        )
    )

    expect_equal(
        exQMatrix(bf_convex_combinations_of_bfs, d, cscale = cscale),
        ex_qmatrix_naive(
            actual_fn, d,
            coefficients = bf_convex_combinations_of_bfs@coefficients,
            points = bf_convex_combinations_of_bfs@points,
            cscale = cscale
        )
    )

    skip_if_not(has_levy_density(bf_convex_combinations_of_bfs))
    expect_equal(
        exQMatrix(bf_convex_combinations_of_bfs, d, cscale = cscale),
        exQMatrix(
            bf_convex_combinations_of_bfs, d,
            cscale = cscale,
            method = "levy", tolerance = testthat_tolerance()
        )
    )

    skip_if_not(has_stieltjes_density(bf_convex_combinations_of_bfs))
    expect_equal(
        exQMatrix(bf_convex_combinations_of_bfs, d, cscale = cscale),
        exQMatrix(
            bf_convex_combinations_of_bfs, d,
            cscale = cscale,
            method = "stieltjes", tolerance = testthat_tolerance()
        )
    )
})
