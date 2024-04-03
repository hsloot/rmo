bf2intensities <- function(d, bf) {
  intensities(bf, d)
}

bf2ex_intensities <- function(d, bf) {
  exIntensities(bf, d)
}

ex_intensities_constant <- function(d, constant) {
  bf <- ConstantBernsteinFunction(constant = constant)
  bf2ex_intensities(d, bf)
}

intensities_constant <- function(d, constant) {
  bf <- ConstantBernsteinFunction(constant = constant)
  bf2intensities(d, bf)
}

ex_intensities_linear <- function(d, scale) {
  bf <- LinearBernsteinFunction(scale = scale)
  bf2ex_intensities(d, bf)
}

intensities_linear <- function(d, scale) {
  bf <- LinearBernsteinFunction(scale = scale)
  bf2intensities(d, bf)
}

ex_intensities_armageddon <- function(d, alpha, beta) {
  ex_intensities_linear(d, scale = alpha) +
    ex_intensities_constant(d, constant = beta)
}

intensities_armageddon <- function(d, alpha, beta) {
  intensities_linear(d, scale = alpha) +
    intensities_constant(d, constant = beta)
}

ex_intensities_poisson <- function(d, eta) {
  bf <- PoissonBernsteinFunction(eta = eta)
  bf2ex_intensities(d, bf)
}

intensities_poisson <- function(d, eta) {
  bf <- PoissonBernsteinFunction(eta = eta)
  bf2intensities(d, bf)
}

ex_intensities_alpha_stable <- function(d, alpha) {
  bf <- AlphaStableBernsteinFunction(alpha = alpha)
  bf2ex_intensities(d, bf)
}

intensities_alpha_stable <- function(d, alpha) {
  bf <- AlphaStableBernsteinFunction(alpha = alpha)
  bf2intensities(d, bf)
}

ex_intensities_exponential <- function(d, lambda) {
  bf <- ExponentialBernsteinFunction(lambda = lambda)
  bf2ex_intensities(d, bf)
}

intensities_exponential <- function(d, lambda) {
  bf <- ExponentialBernsteinFunction(lambda = lambda)
  bf2intensities(d, bf)
}

ex_intensities_gamma <- function(d, a) {
  bf <- GammaBernsteinFunction(a = a)
  bf2ex_intensities(d, bf)
}

intensities_gamma <- function(d, a) {
  bf <- GammaBernsteinFunction(a = a)
  bf2intensities(d, bf)
}

ex_intensities_pareto <- function(d, alpha, x0) {
  bf <- ParetoBernsteinFunction(alpha = alpha, x0 = x0)
  bf2ex_intensities(d, bf)
}

intensities_pareto <- function(d, alpha, x0) {
  bf <- ParetoBernsteinFunction(alpha = alpha, x0 = x0)
  bf2intensities(d, bf)
}

ex_intensities_inverse_gaussian <- function(d, eta) { # nolint
  bf <- InverseGaussianBernsteinFunction(eta = eta)
  bf2ex_intensities(d, bf)
}

intensities_inverse_gaussian <- function(d, eta) {
  bf <- InverseGaussianBernsteinFunction(eta = eta)
  bf2intensities(d, bf)
}

intensities_hierarchical <- function(d1, d2, lambda, eta, a, alpha) { # nolint
  ex_intensities_1 <- ex_intensities_gamma(d1, a)
  ex_intensities_2 <- ex_intensities_alpha_stable(d2, alpha)

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
      intensities[j] <- intensities[j] + ex_intensities_1[count_1]
    } else if (count_2 > 0 && count_1 == 0) {
      intensities[j] <- intensities[j] + ex_intensities_2[count_2]
    }
  }

  intensities
}
