use_seed <- 1632
n <- 1e2

rmo_am <- function(...) {
  mockery::stub(rmo, "Rcpp__rmo_am", rtest__rmo_am)
  rmo(..., method = "AM")
}

## #### Test implementation for the bivariate case ####

test_that("Arnold model implementation for d = 2", {
  ## all equal
  args <- list(
    "d" = 2L,
    "lambda" = c(1, 1, 1)
  )
  expect_equal_rn_generation(
    rmo_am, testutils.rmo::rmo_am_naive,
    args, n, use_seed
  )

  ## independence
  args <- list(
    "d" = 2,
    "lambda" = calc_lambda_linear(2, scale = 0.7)
  )
  expect_equal_rn_generation(
    rmo_am, testutils.rmo::rmo_am_naive,
    args, n, use_seed
  )

  ## comonotone
  args <- list(
    "d" = 2,
    "lambda" = calc_lambda_constant(2, constant = 0.7)
  )
  expect_equal_rn_generation(
    rmo_am, testutils.rmo::rmo_am_naive,
    args, n, use_seed
  )

  ## Poisson
  args <- list(
    "d" = 2,
    "lambda" = calc_lambda_poisson(
      2,
      eta = 0.3
    )
  )
  expect_equal_rn_generation(
    rmo_am, testutils.rmo::rmo_am_naive,
    args, n, use_seed
  )

  ## Alpha-Stable
  args <- list(
    "d" = 2,
    "lambda" = calc_lambda_alpha_stable(2, alpha = 0.25)
  )
  expect_equal_rn_generation(
    rmo_am, testutils.rmo::rmo_am_naive,
    args, n, use_seed
  )

  ## Gamma
  args <- list(
    "d" = 2,
    "lambda" = calc_lambda_gamma(2, a = 0.4)
  )
  expect_equal_rn_generation(
    rmo_am, testutils.rmo::rmo_am_naive,
    args, n, use_seed
  )

  ## Pareto
  args <- list(
    "d" = 2,
    "lambda" = calc_lambda_pareto(2, alpha = 0.4, x0 = 1e-4)
  )
  expect_equal_rn_generation(
    rmo_am, testutils.rmo::rmo_am_naive,
    args, n, use_seed
  )

  ## Inverse-Gaussian
  args <- list(
    "d" = 2,
    "lambda" = calc_lambda_inverse_gaussian(2, eta = 0.5)
  )
  expect_equal_rn_generation(
    rmo_am, testutils.rmo::rmo_am_naive,
    args, n, use_seed
  )

  ## Mixed
  args <- list(
    "d" = 2,
    "lambda" = c(0.1, 0.005, 2)
  )
  expect_equal_rn_generation(
    rmo_am, testutils.rmo::rmo_am_naive,
    args, n, use_seed
  )
})


## ## test implementation against original `R` version ####

test_that("Arnold model implementation for d>2", {
  ## dimension parameters
  d1 <- 3
  d2 <- 4
  d <- d1 + d2

  ## all equal
  args <- list(
    "d" = d,
    "lambda" = rep(0.5, times = 2^d - 1)
  )
  expect_equal_rn_generation(
    rmo_am, testutils.rmo::rmo_am_naive,
    args, n, use_seed
  )

  ## independence
  args <- list(
    "d" = d,
    "lambda" = calc_lambda_linear(d, scale = 0.7)
  )
  expect_equal_rn_generation(
    rmo_am, testutils.rmo::rmo_am_naive,
    args, n, use_seed
  )

  ## comonotone
  args <- list(
    "d" = d,
    "lambda" = calc_lambda_constant(d, constant = 0.7)
  )
  expect_equal_rn_generation(
    rmo_am, testutils.rmo::rmo_am_naive,
    args, n, use_seed
  )

  ## Poisson
  args <- list(
    "d" = d,
    "lambda" = calc_lambda_poisson(
      d,
      eta = 0.3
    )
  )
  expect_equal_rn_generation(
    rmo_am, testutils.rmo::rmo_am_naive,
    args, n, use_seed
  )

  ## Alpha-stable
  args <- list(
    "d" = d,
    "lambda" = calc_lambda_alpha_stable(d, alpha = 0.25)
  )
  expect_equal_rn_generation(
    rmo_am, testutils.rmo::rmo_am_naive,
    args, n, use_seed
  )

  ## Gamma
  args <- list(
    "d" = d,
    "lambda" = calc_lambda_gamma(d, a = 0.4)
  )
  expect_equal_rn_generation(
    rmo_am, testutils.rmo::rmo_am_naive,
    args, n, use_seed
  )

  ## Pareto
  args <- list(
    "d" = d,
    "lambda" = calc_lambda_pareto(d, alpha = 0.4, x0 = 1e-4)
  )
  expect_equal_rn_generation(
    rmo_am, testutils.rmo::rmo_am_naive,
    args, n, use_seed
  )

  ## Inverse-Gaussian
  args <- list(
    "d" = d,
    "lambda" = calc_lambda_inverse_gaussian(d, eta = 0.5)
  )
  expect_equal_rn_generation(
    rmo_am, testutils.rmo::rmo_am_naive,
    args, n, use_seed
  )

  ## Hierarchical
  args <- list(
    "d" = d,
    "lambda" = calc_lambda_hierarchical(
      d1 = d1, d2 = d2, gamma = 0.1, eta = 0.3,
      a = 0.2, alpha = 0.4
    )
  )
  expect_equal_rn_generation(
    rmo_am, testutils.rmo::rmo_am_naive,
    args, n, use_seed
  )
})

# #### Test no-sample ####

test_that("AM implementation for n = 0", {
  n <- 0
  d <- 5
  x <- rmo_am(n, d, calc_lambda_exponential(d, 1))
  checkmate::expect_matrix(x, mode = "numeric", nrows = n, ncols = d)
})
