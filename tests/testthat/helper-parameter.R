calc_theta_constant <- function(d, constant) {
  bf <- ConstantBernsteinFunction(constant = constant)
  calcExShockSizeArrivalIntensities(bf, d)
}

intensities_constant <- function(d, constant) {
  bf <- ConstantBernsteinFunction(constant = constant)
  intensities(bf, d)
}

calc_theta_linear <- function(d, scale) {
  bf <- LinearBernsteinFunction(scale = scale)
  calcExShockSizeArrivalIntensities(bf, d)
}

intensities_linear <- function(d, scale) {
  bf <- LinearBernsteinFunction(scale = scale)
  intensities(bf, d)
}

calc_theta_armageddon <- function(d, alpha, beta) {
  calc_theta_linear(d, scale = alpha) +
    calc_theta_constant(d, constant = beta)
}

intensities_armageddon <- function(d, alpha, beta) {
  intensities_linear(d, scale = alpha) +
    intensities_constant(d, constant = beta)
}

calc_theta_poisson <- function(d, eta) {
  bf <- PoissonBernsteinFunction(eta = eta)
  calcExShockSizeArrivalIntensities(bf, d)
}

intensities_poisson <- function(d, eta) {
  bf <- PoissonBernsteinFunction(eta = eta)
  intensities(bf, d)
}

calc_theta_alpha_stable <- function(d, alpha) {
  bf <- AlphaStableBernsteinFunction(alpha = alpha)
  calcExShockSizeArrivalIntensities(bf, d)
}

intensities_alpha_stable <- function(d, alpha) {
  bf <- AlphaStableBernsteinFunction(alpha = alpha)
  intensities(bf, d)
}

calc_theta_exponential <- function(d, lambda) {
  bf <- ExponentialBernsteinFunction(lambda = lambda)
  calcExShockSizeArrivalIntensities(bf, d)
}

intensities_exponential <- function(d, lambda) {
  bf <- ExponentialBernsteinFunction(lambda = lambda)
  intensities(bf, d)
}

calc_theta_gamma <- function(d, a) {
  bf <- GammaBernsteinFunction(a = a)
  calcExShockSizeArrivalIntensities(bf, d)
}

intensities_gamma <- function(d, a) {
  bf <- GammaBernsteinFunction(a = a)
  intensities(bf, d)
}

calc_theta_pareto <- function(d, alpha, x0) {
  bf <- ParetoBernsteinFunction(alpha = alpha, x0 = x0)
  calcExShockSizeArrivalIntensities(bf, d)
}

intensities_pareto <- function(d, alpha, x0) {
  bf <- ParetoBernsteinFunction(alpha = alpha, x0 = x0)
  intensities(bf, d)
}

calc_theta_inverse_gaussian <- function(d, eta) { # nolint
  bf <- InverseGaussianBernsteinFunction(eta = eta)
  calcExShockSizeArrivalIntensities(bf, d)
}

intensities_inverse_gaussian <- function(d, eta) {
  bf <- InverseGaussianBernsteinFunction(eta = eta)
  intensities(bf, d)
}

intensities_hierarchical <- function(d1, d2, lambda, eta, a, alpha) { # nolint
  theta_1 <- calc_theta_gamma(d1, a)
  theta_2 <- calc_theta_alpha_stable(d2, alpha)

  intensities <- lambda * intensities_poisson(d1 + d2, eta)
  for (j in seq_along(intensities)) {
    count_1 <- 0
    count_2 <- 0
    for (i in 1:d1) {
      count_1 <- count_1 + Rcpp__is_within(i, j)
    }
    for (i in 1:d2) {
      count_2 <- count_2 + Rcpp__is_within(d1 + i, j)
    }

    if (count_1 > 0 && count_2 == 0) {
      intensities[j] <- intensities[j] + theta_1[count_1]
    } else if (count_2 > 0 && count_1 == 0) {
      intensities[j] <- intensities[j] + theta_2[count_2]
    }
  }

  intensities
}
