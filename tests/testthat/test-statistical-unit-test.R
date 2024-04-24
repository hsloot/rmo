## #### Global settings ####
n <- 1e3
set.seed(1623)

rmo_esm <- function(...) {
  rmo(..., method = "ESM")
}

rmo_am <- function(...) {
  rmo(..., method = "AM")
}

rexmo_mdcm <- function(...) {
  rexmo(..., method = "MDCM")
}

rarmextmo_esm <- function(n, d, alpha, beta) {
  rpextmo(n, d, a = beta, b = alpha, family = "Armageddon", method = "ESM")
}

rextmo_lfm <- function(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list) { # nolint
  if (isTRUE(rate == 0) ||
        (isTRUE(rjump_name == "rposval") &&
           isTRUE(rjump_arg_list$value == 0))) {
    rpextmo(
      n, d,
      a = rate_killing, b = rate_drift,
      family = "Armageddon", method = "LFM"
    )
  } else if (isTRUE("rposval" == rjump_name)) {
    rpextmo(
      n, d,
      a = rate_killing, b = rate_drift, gamma = rate,
      eta = rjump_arg_list$value,
      family = "Poisson", method = "LFM"
    )
  } else if (isTRUE("rpareto" == rjump_name)) {
    rpextmo(
      n, d,
      a = rate_killing, b = rate_drift, gamma = rate,
      eta = c(rjump_arg_list$alpha, rjump_arg_list$x0),
      family = "Pareto", method = "LFM"
    )
  } else if (isTRUE("rexp" == rjump_name)) {
    rpextmo(
      n, d,
      a = rate_killing, b = rate_drift, gamma = rate,
      eta = rjump_arg_list$rate,
      family = "Exponential", method = "LFM"
    )
  } else {
    stop(sprintf("Jump distribution %s not implemented", rjump_name))
  }
}

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
ex_ks_test <- function(n, d, FUN, theta) { # nolint
  min_rate <- sum(theta)
  x <- min_rate * apply(FUN(n, d, theta), 1, min)
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
    bf <- PoissonBernsteinFunction(eta = rjump_arg_list$value)
  } else if ("rexp" == rjump_name) {
    bf <- ExponentialBernsteinFunction(lambda = rjump_arg_list$rate)
  } else if ("rpareto" == rjump_name) {
    bf <- ParetoBernsteinFunction(
      alpha = rjump_arg_list$alpha,
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
lfm_ks_test <- function(n, d, FUN, # nolint
                        rate, rate_killing, rate_drift,
                        rjump_name, rjump_arg_list) {
  bf <- lfm_bf(rate, rate_killing, rate_drift, rjump_name, rjump_arg_list)
  min_rate <- valueOf(bf, d, 0L)
  x <- min_rate * apply(
    FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list),
    1, min
  )
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

  bf <- SumOfBernsteinFunctions(
    first = ConstantBernsteinFunction(constant = 2.73383),
    second = LinearBernsteinFunction(scale = 2.296823)
  )
  intensities <- intensities(bf, d)
  expect_not_rejected(
    ks_test(n, d, rmo_esm, intensities),
    unit_threshold
  )

  bf <- AlphaStableBernsteinFunction(alpha = 0.595094)
  intensities <- intensities(bf, d)
  expect_not_rejected(
    ks_test(n, d, rmo_esm, intensities),
    unit_threshold
  )

  bf <- InverseGaussianBernsteinFunction(eta = 0.5186811)
  intensities <- intensities(bf, d)
  expect_not_rejected(
    ks_test(n, d, rmo_esm, intensities),
    unit_threshold
  )

  bf <- ExponentialBernsteinFunction(lambda = 0.3410571)
  intensities <- intensities(bf, d)
  expect_not_rejected(
    ks_test(n, d, rmo_esm, intensities),
    unit_threshold
  )
})



## #### Arnold model ####

test_that("Arnold model passes statistical unit test", {
  unit_threshold <- arnold_threshold / 4
  d <- 15

  bf <- SumOfBernsteinFunctions(
    first = ConstantBernsteinFunction(constant = 0.1819954),
    second = LinearBernsteinFunction(scale = 1.152357)
  )
  intensities <- intensities(bf, d)
  expect_not_rejected(
    ks_test(n, d, rmo_am, intensities),
    unit_threshold
  )

  bf <- AlphaStableBernsteinFunction(alpha = 0.6546152)
  intensities <- intensities(bf, d)
  expect_not_rejected(
    ks_test(n, d, rmo_am, intensities),
    unit_threshold
  )

  bf <- InverseGaussianBernsteinFunction(eta = 0.09929306)
  intensities <- intensities(bf, d)
  expect_not_rejected(
    ks_test(n, d, rmo_am, intensities),
    unit_threshold
  )

  bf <- ExponentialBernsteinFunction(lambda = 0.6385946)
  intensities <- intensities(bf, d)
  expect_not_rejected(
    ks_test(n, d, rmo_am, intensities),
    unit_threshold
  )
})


## #### Exchangeable Arnold model ####

test_that("Exchangeable Arnold model passes statistical unit test", {
  unit_threshold <- ex_arnold_threshold / 8
  d <- 15

  bf <- SumOfBernsteinFunctions(
    first = ConstantBernsteinFunction(constant = 2.112945),
    second = LinearBernsteinFunction(scale = 0.5088719)
  )
  theta <- calcExShockSizeArrivalIntensities(bf, d)
  expect_not_rejected(
    ex_ks_test(n, d, rexmo_mdcm, theta),
    unit_threshold
  )

  bf <- AlphaStableBernsteinFunction(alpha = 0.09870832)
  theta <- calcExShockSizeArrivalIntensities(bf, d)
  expect_not_rejected(
    ex_ks_test(n, d, rexmo_mdcm, theta),
    unit_threshold
  )

  bf <- InverseGaussianBernsteinFunction(eta = 4.687992)
  theta <- calcExShockSizeArrivalIntensities(bf, d)
  expect_not_rejected(
    ex_ks_test(n, d, rexmo_mdcm, theta),
    unit_threshold
  )

  bf <- ExponentialBernsteinFunction(lambda = 0.2047898)
  theta <- calcExShockSizeArrivalIntensities(bf, d)
  expect_not_rejected(
    ex_ks_test(n, d, rexmo_mdcm, theta),
    unit_threshold
  )

  d <- 125

  bf <- SumOfBernsteinFunctions(
    first = ConstantBernsteinFunction(constant = 1.796975),
    second = LinearBernsteinFunction(scale = 0.3058118)
  )
  theta <- calcExShockSizeArrivalIntensities(bf, d)
  expect_not_rejected(
    ex_ks_test(n, d, rexmo_mdcm, theta),
    unit_threshold
  )

  bf <- AlphaStableBernsteinFunction(alpha = 0.9034687)
  theta <- calcExShockSizeArrivalIntensities(bf, d)
  expect_not_rejected(
    ex_ks_test(n, d, rexmo_mdcm, theta),
    unit_threshold
  )

  bf <- InverseGaussianBernsteinFunction(eta = 5.023343)
  theta <- calcExShockSizeArrivalIntensities(bf, d)
  expect_not_rejected(
    ex_ks_test(n, d, rexmo_mdcm, theta),
    unit_threshold
  )

  bf <- ExponentialBernsteinFunction(lambda = 0.6293527)
  theta <- calcExShockSizeArrivalIntensities(bf, d)
  expect_not_rejected(
    ex_ks_test(n, d, rexmo_mdcm, theta),
    unit_threshold
  )
})


## #### Armageddon ESM ####

test_that("Armageddon ESM passes statistical unit test", {
  unit_threshold <- armageddon_extmo_threshold / 2
  d <- 15

  bf <- SumOfBernsteinFunctions(
    first = ConstantBernsteinFunction(constant = 0.1712597),
    second = LinearBernsteinFunction(scale = 1.012998)
  )
  alpha <- bf@second@scale
  beta <- bf@first@constant
  expect_not_rejected(
    armageddon_extmo_ks_test(n, d, rarmextmo_esm, alpha, beta),
    unit_threshold
  )

  d <- 125

  bf <- SumOfBernsteinFunctions(
    first = ConstantBernsteinFunction(constant = 0.5255554),
    second = LinearBernsteinFunction(scale = 1.696983)
  )
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

  bf <- SumOfBernsteinFunctions(
    first = ConstantBernsteinFunction(constant = 2.026176),
    second = SumOfBernsteinFunctions(
      first = LinearBernsteinFunction(scale = 2.528583),
      second = PoissonBernsteinFunction(eta = 1.523929)
    )
  )
  rate_drift <- bf@second@first@scale
  rate_killing <- bf@first@constant
  rate <- 1
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
  bf <- SumOfBernsteinFunctions(
    first = ConstantBernsteinFunction(constant = 3.658762),
    second = SumOfBernsteinFunctions(
      first = LinearBernsteinFunction(scale = 1.143383),
      second = ScaledBernsteinFunction(
        scale = 0.3249725,
        original = ParetoBernsteinFunction(
          alpha = 0.5480305,
          x0 = 2
        )
      )
    )
  )
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

  bf <- SumOfBernsteinFunctions(
    first = ConstantBernsteinFunction(constant = 1.786405),
    second = SumOfBernsteinFunctions(
      first = LinearBernsteinFunction(scale = 2.162608),
      second = ScaledBernsteinFunction(
        scale = 0.08321866,
        original = ExponentialBernsteinFunction(lambda = 1.17918)
      )
    )
  )
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

  bf <- SumOfBernsteinFunctions(
    first = ConstantBernsteinFunction(constant = 0.2450249),
    second = SumOfBernsteinFunctions(
      first = LinearBernsteinFunction(scale = 1.041612),
      second = PoissonBernsteinFunction(eta = 1.790146)
    )
  )
  rate_drift <- bf@second@first@scale
  rate_killing <- bf@first@constant
  rate <- 1
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

  bf <- SumOfBernsteinFunctions(
    first = ConstantBernsteinFunction(constant = 0.3258317),
    second = SumOfBernsteinFunctions(
      first = LinearBernsteinFunction(scale = 0.01834995),
      second = ScaledBernsteinFunction(
        scale = 0.1350263,
        original = ParetoBernsteinFunction(
          alpha = 0.2507382,
          x0 = 1e-04
        )
      )
    )
  )
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

  bf <- SumOfBernsteinFunctions(
    first = ConstantBernsteinFunction(constant = 0.2251528),
    second = SumOfBernsteinFunctions(
      first = LinearBernsteinFunction(scale = 0.9185561),
      second = ScaledBernsteinFunction(
        scale = 0.07279246,
        original = ExponentialBernsteinFunction(lambda = 0.7327681)
      )
    )
  )
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
