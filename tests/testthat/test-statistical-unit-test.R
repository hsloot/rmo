## #### Global settings ####
n <- 1e3
set.seed(1623)

## All threshold should be chosen such that, in total, they sum up to this
## number. If this is, e.g., 1%, then the probability of a false positive,
## assuming the null hypothesis is fulfilled, is 1%.
total_threshold <- 1e-2
esm_threshold <- total_threshold / 25
arnold_threshold <- total_threshold / 25 * 3
ex_arnold_threshold <- total_threshold / 25 * 10
armageddon_extmo_threshold <- total_threshold / 25
lfm_cpp_threshold <- total_threshold / 25 * 10


## #### Kolmogorov-Smirnov tests ####

#' @importFrom stats ks.test
#' @noRd
#' @keywords internal test
ks_test <- function(n, d, FUN, intensities) { # nolint
  min_rate <- sum(intensities)
  x <- min_rate * apply(FUN(n, d, intensities), 1, min)
  ks.test(x, stats::pexp)
}

#' @importFrom stats ks.test
#' @noRd
#' @keywords internal test
ex_ks_test <- function(n, d, FUN, ex_intensities) { # nolint
  min_rate <- sum(ex_intensities)
  x <- min_rate * apply(FUN(n, d, ex_intensities), 1, min)
  ks.test(x, stats::pexp)
}

#' @importFrom stats ks.test
#' @noRd
#' @keywords internal test
armageddon_extmo_ks_test <- function(n, d, FUN, alpha, beta) { # nolint
  min_rate <- (alpha * d + beta)
  x <- min_rate * apply(FUN(n, d, alpha, beta), 1, min)
  ks.test(x, stats::pexp)
}

#' @noRd
#' @keywords internal test
lfm_bf <- function(rate, rate_killing, rate_drift, rjump_name, rjump_arg_list) {
  if ("rposval" == rjump_name) {
    bf <- PoissonBernsteinFunction(lambda = 1, eta = rjump_arg_list$value)
  } else if ("rexp" == rjump_name) {
    bf <- ExponentialBernsteinFunction(lambda = rjump_arg_list$rate)
  } else if ("rpareto" == rjump_name) {
    bf <- ParetoBernsteinFunction(
      alpha =  rjump_arg_list$alpha,
      x0 = rjump_arg_list$x0
    )
  }
  bf <- ScaledBernsteinFunction(scale = rate, original = bf)
  bf <- SumOfBernsteinFunctions(
    first = LinearBernsteinFunction(scale = rate_drift),
    second = bf
  )
  SumOfBernsteinFunctions(
    first = ConstantBernsteinFunction(constant = rate_killing),
    second = bf
  )
}

#' @importFrom stats ks.test
#' @noRd
#' @keywords internal test
lfm_ks_test <- function(
    n, d, FUN, # nolint
    rate, rate_killing, rate_drift,
    rjump_name, rjump_arg_list) {
  bf <- lfm_bf(rate, rate_killing, rate_drift, rjump_name, rjump_arg_list)
  min_rate <- valueOf(bf, d, 0L)
  x <- min_rate * apply(
    FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list),
    1, min)
  ks.test(x, stats::pexp)
}

#' @importFrom testthat expect quasi_label
#' @importFrom rlang enquo
#' @noRd
#' @keywords internal test
expect_not_rejected <- function(htest, threshold) {
  htest <- testthat::quasi_label(rlang::enquo(htest), arg = "htest")
  testthat::expect(
    ok = htest$val$p.value >= threshold,
    failure_message = sprintf(
      "Null hypothesis rejected %e < %e", htest$val$p.value, threshold
    )
  )

  invisible(TRUE)
}

## #### ESM ####

test_that("ESM passes statistical unit test", {
  unit_threshold <- esm_threshold / 4
  d <- 7

  bf <- testutils.rmo::fuzzy_bf(SumOfBernsteinFunctions(
    first = ConstantBernsteinFunction(),
    second = LinearBernsteinFunction()
  ))
  intensities <- bf2intensities(d, bf)
  expect_not_rejected(
    ks_test(n, d, rmo_esm, intensities),
    unit_threshold
  )

  bf <- testutils.rmo::fuzzy_bf(AlphaStableBernsteinFunction())
  intensities <- bf2intensities(d, bf)
  expect_not_rejected(
    ks_test(n, d, rmo_esm, intensities),
    unit_threshold
  )

  bf <- testutils.rmo::fuzzy_bf(InverseGaussianBernsteinFunction())
  intensities <- bf2intensities(d, bf)
  expect_not_rejected(
    ks_test(n, d, rmo_esm, intensities),
    unit_threshold
  )

  bf <- testutils.rmo::fuzzy_bf(ExponentialBernsteinFunction())
  intensities <- bf2intensities(d, bf)
  expect_not_rejected(
    ks_test(n, d, rmo_esm, intensities),
    unit_threshold
  )
})



## #### Arnold model ####

test_that("Arnold model passes statistical unit test", {
  unit_threshold <- arnold_threshold / 4
  d <- 15

  bf <- testutils.rmo::fuzzy_bf(SumOfBernsteinFunctions(
    first = ConstantBernsteinFunction(),
    second = LinearBernsteinFunction()
  ))
  intensities <- bf2intensities(d, bf)
  expect_not_rejected(
    ks_test(n, d, rmo_arnold, intensities),
    unit_threshold
  )

  bf <- testutils.rmo::fuzzy_bf(AlphaStableBernsteinFunction())
  intensities <- bf2intensities(d, bf)
  expect_not_rejected(
    ks_test(n, d, rmo_arnold, intensities),
    unit_threshold
  )

  bf <- testutils.rmo::fuzzy_bf(InverseGaussianBernsteinFunction())
  intensities <- bf2intensities(d, bf)
  expect_not_rejected(
    ks_test(n, d, rmo_arnold, intensities),
    unit_threshold
  )

  bf <- testutils.rmo::fuzzy_bf(ExponentialBernsteinFunction())
  intensities <- bf2intensities(d, bf)
  expect_not_rejected(
    ks_test(n, d, rmo_arnold, intensities),
    unit_threshold
  )
})


## #### Exchangeable Arnold model ####

test_that("Exchangeable Arnold model passes statistical unit test", {
  unit_threshold <- ex_arnold_threshold / 8
  d <- 15

  bf <- testutils.rmo::fuzzy_bf(SumOfBernsteinFunctions(
    first = ConstantBernsteinFunction(),
    second = LinearBernsteinFunction()
  ))
  ex_intensities <- bf2ex_intensities(d, bf)
  expect_not_rejected(
    ex_ks_test(n, d, rexmo_markovian, ex_intensities),
    unit_threshold
  )

  bf <- testutils.rmo::fuzzy_bf(AlphaStableBernsteinFunction())
  ex_intensities <- bf2ex_intensities(d, bf)
  expect_not_rejected(
    ex_ks_test(n, d, rexmo_markovian, ex_intensities),
    unit_threshold
  )

  bf <- testutils.rmo::fuzzy_bf(InverseGaussianBernsteinFunction())
  ex_intensities <- bf2ex_intensities(d, bf)
  expect_not_rejected(
    ex_ks_test(n, d, rexmo_markovian, ex_intensities),
    unit_threshold
  )

  bf <- testutils.rmo::fuzzy_bf(ExponentialBernsteinFunction())
  ex_intensities <- bf2ex_intensities(d, bf)
  expect_not_rejected(
    ex_ks_test(n, d, rexmo_markovian, ex_intensities),
    unit_threshold
  )

  d <- 125

  bf <- testutils.rmo::fuzzy_bf(SumOfBernsteinFunctions(
    first = ConstantBernsteinFunction(),
    second = LinearBernsteinFunction()
  ))
  ex_intensities <- bf2ex_intensities(d, bf)
  expect_not_rejected(
    ex_ks_test(n, d, rexmo_markovian, ex_intensities),
    unit_threshold
  )

  bf <- testutils.rmo::fuzzy_bf(AlphaStableBernsteinFunction())
  ex_intensities <- bf2ex_intensities(d, bf)
  expect_not_rejected(
    ex_ks_test(n, d, rexmo_markovian, ex_intensities),
    unit_threshold
  )

  bf <- testutils.rmo::fuzzy_bf(InverseGaussianBernsteinFunction())
  ex_intensities <- bf2ex_intensities(d, bf)
  expect_not_rejected(
    ex_ks_test(n, d, rexmo_markovian, ex_intensities),
    unit_threshold
  )

  bf <- testutils.rmo::fuzzy_bf(ExponentialBernsteinFunction())
  ex_intensities <- bf2ex_intensities(d, bf)
  expect_not_rejected(
    ex_ks_test(n, d, rexmo_markovian, ex_intensities),
    unit_threshold
  )
})


## #### Armageddon ESM ####

test_that("Armageddon ESM passes statistical unit test", {
  unit_threshold <- armageddon_extmo_threshold / 2
  d <- 15

  bf <- testutils.rmo::fuzzy_bf(SumOfBernsteinFunctions(
    first = ConstantBernsteinFunction(),
    second = LinearBernsteinFunction()
  ))
  alpha <- bf@second@scale
  beta <- bf@first@constant
  expect_not_rejected(
    armageddon_extmo_ks_test(n, d, rarmextmo_esm, alpha, beta),
    unit_threshold
  )

  d <- 125

  bf <- testutils.rmo::fuzzy_bf(SumOfBernsteinFunctions(
    first = ConstantBernsteinFunction(),
    second = LinearBernsteinFunction()
  ))
  alpha <- bf@second@scale
  beta <- bf@first@constant
  expect_not_rejected(
    armageddon_extmo_ks_test(n, d, rarmextmo_esm, alpha, beta),
    unit_threshold
  )
})


## #### Lévy-frailty CPP model ####

test_that("Lévy frailty CPP model passes statistical unit test", {
  unit_threshold <- lfm_cpp_threshold / 6
  d <- 15

  bf <- PoissonBernsteinFunction()
  bf <- SumOfBernsteinFunctions(first = LinearBernsteinFunction(), second = bf)
  bf <- SumOfBernsteinFunctions(first = ConstantBernsteinFunction(), second = bf)
  bf <- testutils.rmo::fuzzy_bf(bf)
  rate_drift <- bf@second@first@scale
  rate_killing <- bf@first@constant
  rate <- bf@second@second@lambda
  rjump_name <- "rposval"
  rjump_arg_list <- list("value" = bf@second@second@eta)
  expect_not_rejected(
    lfm_ks_test(
      n, d, rextmo_lfm,
      rate, rate_killing, rate_drift,
      rjump_name, rjump_arg_list
    ),
    unit_threshold
  )

  bf <- ParetoBernsteinFunction()
  bf <- ScaledBernsteinFunction(scale = 1, original = bf)
  bf <- SumOfBernsteinFunctions(first = LinearBernsteinFunction(), second = bf)
  bf <- SumOfBernsteinFunctions(first = ConstantBernsteinFunction(), second = bf)
  bf <- testutils.rmo::fuzzy_bf(bf)
  rate_drift <- bf@second@first@scale
  rate_killing <- bf@first@constant
  rate <- bf@second@second@scale
  rjump_name <- "rpareto"
  rjump_arg_list <- list(
    "alpha" = bf@second@second@original@alpha,
    "x0" = bf@second@second@original@x0
  )
  expect_not_rejected(
    lfm_ks_test(
      n, d, rextmo_lfm,
      rate, rate_killing, rate_drift,
      rjump_name, rjump_arg_list
    ),
    unit_threshold
  )

  bf <- ExponentialBernsteinFunction()
  bf <- ScaledBernsteinFunction(scale = 1, original = bf)
  bf <- SumOfBernsteinFunctions(first = LinearBernsteinFunction(), second = bf)
  bf <- SumOfBernsteinFunctions(first = ConstantBernsteinFunction(), second = bf)
  bf <- testutils.rmo::fuzzy_bf(bf)
  rate_drift <- bf@second@first@scale
  rate_killing <- bf@first@constant
  rate <- bf@second@second@scale
  rjump_name <- "rexp"
  rjump_arg_list <- list("rate" = bf@second@second@original@lambda)
  expect_not_rejected(
    lfm_ks_test(
      n, d, rextmo_lfm,
      rate, rate_killing, rate_drift,
      rjump_name, rjump_arg_list
    ),
    unit_threshold
  )

  d <- 125

  bf <- PoissonBernsteinFunction()
  bf <- SumOfBernsteinFunctions(first = LinearBernsteinFunction(), second = bf)
  bf <- SumOfBernsteinFunctions(first = ConstantBernsteinFunction(), second = bf)
  bf <- testutils.rmo::fuzzy_bf(bf)
  rate_drift <- bf@second@first@scale
  rate_killing <- bf@first@constant
  rate <- bf@second@second@lambda
  rjump_name <- "rposval"
  rjump_arg_list <- list("value" = bf@second@second@eta)
  expect_not_rejected(
    lfm_ks_test(
      n, d, rextmo_lfm,
      rate, rate_killing, rate_drift,
      rjump_name, rjump_arg_list
    ),
    unit_threshold
  )

  bf <- ParetoBernsteinFunction()
  bf <- ScaledBernsteinFunction(scale = 1, original = bf)
  bf <- SumOfBernsteinFunctions(first = LinearBernsteinFunction(), second = bf)
  bf <- SumOfBernsteinFunctions(first = ConstantBernsteinFunction(), second = bf)
  bf <- testutils.rmo::fuzzy_bf(bf)
  rate_drift <- bf@second@first@scale
  rate_killing <- bf@first@constant
  rate <- bf@second@second@scale
  rjump_name <- "rpareto"
  rjump_arg_list <- list(
    "alpha" = bf@second@second@original@alpha,
    "x0" = bf@second@second@original@x0
  )
  expect_not_rejected(
    lfm_ks_test(
      n, d, rextmo_lfm,
      rate, rate_killing, rate_drift,
      rjump_name, rjump_arg_list
    ),
    unit_threshold
  )

  bf <- ExponentialBernsteinFunction()
  bf <- ScaledBernsteinFunction(scale = 1, original = bf)
  bf <- SumOfBernsteinFunctions(first = LinearBernsteinFunction(), second = bf)
  bf <- SumOfBernsteinFunctions(first = ConstantBernsteinFunction(), second = bf)
  bf <- testutils.rmo::fuzzy_bf(bf)
  rate_drift <- bf@second@first@scale
  rate_killing <- bf@first@constant
  rate <- bf@second@second@scale
  rjump_name <- "rexp"
  rjump_arg_list <- list("rate" = bf@second@second@original@lambda)
  expect_not_rejected(
    lfm_ks_test(
      n, d, rextmo_lfm,
      rate, rate_killing, rate_drift,
      rjump_name, rjump_arg_list
    ),
    unit_threshold
  )
})
