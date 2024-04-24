bf_convex_combinations_of_bfs <- ConvexCombinationOfBernsteinFunctions(
  coefficients = c(
    1.582807,
    0.3411499,
    1.118504,
    0.3013299,
    0.3344583
  ),
  points = list(
    AlphaStableBernsteinFunction(alpha = 0.9714516),
    InverseGaussianBernsteinFunction(eta = 0.001121208),
    ConstantBernsteinFunction(constant = 0.08010536),
    ParetoBernsteinFunction(alpha = 0.8690643, x0 = 1e-2),
    GammaBernsteinFunction(a = 2.500349)
  )
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

test_that("`calcExShockSizeArrivalIntensities` calculates expected values", {
  expect_equal(
    calcExShockSizeArrivalIntensities(bf_convex_combinations_of_bfs, d),
    calc_ex_shock_size_arrival_intensities_naive(
      actual_fn, d,
      coefficients = bf_convex_combinations_of_bfs@coefficients,
      points = bf_convex_combinations_of_bfs@points
    )
  )

  expect_equal(
    calcExShockSizeArrivalIntensities(
      bf_convex_combinations_of_bfs, d, cscale = cscale
    ),
    calc_ex_shock_size_arrival_intensities_naive(
      actual_fn, d,
      coefficients = bf_convex_combinations_of_bfs@coefficients,
      points = bf_convex_combinations_of_bfs@points,
      cscale = cscale
    )
  )

  skip_if_not(has_levy_density(bf_convex_combinations_of_bfs))
  expect_equal(
    calcExShockSizeArrivalIntensities(
      bf_convex_combinations_of_bfs, d, cscale = cscale
    ),
    calcExShockSizeArrivalIntensities(
      bf_convex_combinations_of_bfs, d,
      cscale = cscale,
      method = "levy",
      tolerance = testthat_tolerance()
    )
  )

  skip_if_not(has_stieltjes_density(bf_convex_combinations_of_bfs))
  expect_equal(
    calcExShockSizeArrivalIntensities(
      bf_convex_combinations_of_bfs, d, cscale = cscale
    ),
    calcExShockSizeArrivalIntensities(
      bf_convex_combinations_of_bfs, d,
      cscale = cscale,
      method = "stieltjes", tolerance = testthat_tolerance()
    )
  )
})

test_that("`calcMDCMGeneratorMatrix` calculates expected values", {
  expect_equal(
    calcMDCMGeneratorMatrix(bf_convex_combinations_of_bfs, d),
    ex_qmatrix_naive(
      actual_fn, d,
      coefficients = bf_convex_combinations_of_bfs@coefficients,
      points = bf_convex_combinations_of_bfs@points
    )
  )

  expect_equal(
    calcMDCMGeneratorMatrix(bf_convex_combinations_of_bfs, d, cscale = cscale),
    ex_qmatrix_naive(
      actual_fn, d,
      coefficients = bf_convex_combinations_of_bfs@coefficients,
      points = bf_convex_combinations_of_bfs@points,
      cscale = cscale
    )
  )

  skip_if_not(has_levy_density(bf_convex_combinations_of_bfs))
  expect_equal(
    calcMDCMGeneratorMatrix(bf_convex_combinations_of_bfs, d, cscale = cscale),
    calcMDCMGeneratorMatrix(
      bf_convex_combinations_of_bfs, d,
      cscale = cscale,
      method = "levy", tolerance = testthat_tolerance()
    )
  )

  skip_if_not(has_stieltjes_density(bf_convex_combinations_of_bfs))
  expect_equal(
    calcMDCMGeneratorMatrix(bf_convex_combinations_of_bfs, d, cscale = cscale),
    calcMDCMGeneratorMatrix(
      bf_convex_combinations_of_bfs, d,
      cscale = cscale,
      method = "stieltjes", tolerance = testthat_tolerance()
    )
  )
})
