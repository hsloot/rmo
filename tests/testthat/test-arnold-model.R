use_seed <- 1632L
n <- 100L

## #### Test implementation for the bivariate case ####

test_that("Arnold model implementation for d = 2", {
  mockery::stub(rmo_arnold, "Rcpp__rmo_arnold", rtest__rmo_arnold)

  ## all equal
  args <- list(
    "d" = 2L,
    "intensities" = c(1, 1, 1)
  )
  expect_equal_rn_generation(
    rmo_arnold, testutils.rmo::rmo_arnold_naive,
    args, n, use_seed)

  ## independence
  args[["intensities"]] <- c(1, 1, 0)
  expect_equal_rn_generation(
    rmo_arnold, testutils.rmo::rmo_arnold_naive,
    args, n, use_seed)

  ## comonotone
  args[["intensities"]] <- c(0, 0, 1)
  expect_equal_rn_generation(
    rmo_arnold, testutils.rmo::rmo_arnold_naive,
    args, n, use_seed)

  ## exchangeable, low, high
  args[["intensities"]] <- c(0.1, 0.1, 2)
  expect_equal_rn_generation(
    rmo_arnold, testutils.rmo::rmo_arnold_naive,
    args, n, use_seed)

  ## exchangeable, high, low
  args[["intensities"]] <- c(3, 3, 0.5)
  expect_equal_rn_generation(
    rmo_arnold, testutils.rmo::rmo_arnold_naive,
    args, n, use_seed)

  ## Low, High, Low
  args[["intensities"]] <- c(0.5, 3, 0.2)
  expect_equal_rn_generation(
    rmo_arnold, testutils.rmo::rmo_arnold_naive,
    args, n, use_seed)

  ## Low, Low, High
  args[["intensities"]] <- c(0.1, 0.005, 2)
  expect_equal_rn_generation(
    rmo_arnold, testutils.rmo::rmo_arnold_naive,
    args, n, use_seed)
})


## ## test implementation against original `R` version ####

test_that("Arnold model implementation for d>2", {
  mockery::stub(rmo_arnold, "Rcpp__rmo_arnold", rtest__rmo_arnold)

  ## dimension parameters
  d1 <- 3L
  d2 <- 4L
  d <- d1 + d2

  ## all equal
  args <- list(
    "d" = d,
    "intensities" = rep(0.5, times=2^d-1)
  )
  expect_equal_rn_generation(
    rmo_arnold, testutils.rmo::rmo_arnold_naive,
    args, n, use_seed)

  ## independence
  args[["intensities"]] <- intensities_constant(d, constant=0.7)
  expect_equal_rn_generation(
    rmo_arnold, testutils.rmo::rmo_arnold_naive,
    args, n, use_seed)

  ## comonotone
  args[["intensities"]] <- intensities_constant(d, constant=0.7)
  expect_equal_rn_generation(
    rmo_arnold, testutils.rmo::rmo_arnold_naive,
    args, n, use_seed)

  ## Poisson
  args[["intensities"]] <- intensities_poisson(d, lambda=0.2, eta=0.3)
  expect_equal_rn_generation(
    rmo_arnold, testutils.rmo::rmo_arnold_naive,
    args, n, use_seed)

  ## Alpha-stable
  args[["intensities"]] <- intensities_alpha_stable(d, alpha=0.25)
  expect_equal_rn_generation(
    rmo_arnold, testutils.rmo::rmo_arnold_naive,
    args, n, use_seed)

  ## Gamma
  args[["intensities"]] <- intensities_alpha_stable(d, a=0.4)
  expect_equal_rn_generation(
    rmo_arnold, testutils.rmo::rmo_arnold_naive,
    args, n, use_seed)

  ## Hierarchical
  args[["intensities"]] <- intensities_hierarchical(
    d1, d2, lambda=0.1, eta=0.3, a=0.2, alpha=0.4)
  expect_equal_rn_generation(
    rmo_arnold, testutils.rmo::rmo_arnold_naive,
    args, n, use_seed)
})

## TODO: add KS unit test
