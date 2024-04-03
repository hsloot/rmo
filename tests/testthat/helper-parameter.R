ex_intensities_constant <- function(d, constant) {
  bf <- ConstantBernsteinFunction(constant = constant)
  exIntensities(bf, d)
}

intensities_constant <- function(d, constant) {
  bf <- ConstantBernsteinFunction(constant = constant)
  intensities(bf, d)
}

ex_intensities_linear <- function(d, scale) {
  bf <- LinearBernsteinFunction(scale = scale)
  exIntensities(bf, d)
}

intensities_linear <- function(d, scale) {
  bf <- LinearBernsteinFunction(scale = scale)
  intensities(bf, d)
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
  exIntensities(bf, d)
}

intensities_poisson <- function(d, eta) {
  bf <- PoissonBernsteinFunction(eta = eta)
  intensities(bf, d)
}

ex_intensities_alpha_stable <- function(d, alpha) {
  bf <- AlphaStableBernsteinFunction(alpha = alpha)
  exIntensities(bf, d)
}

intensities_alpha_stable <- function(d, alpha) {
  bf <- AlphaStableBernsteinFunction(alpha = alpha)
  intensities(bf, d)
}

ex_intensities_exponential <- function(d, lambda) {
  bf <- ExponentialBernsteinFunction(lambda = lambda)
  exIntensities(bf, d)
}

intensities_exponential <- function(d, lambda) {
  bf <- ExponentialBernsteinFunction(lambda = lambda)
  intensities(bf, d)
}

ex_intensities_gamma <- function(d, a) {
  bf <- GammaBernsteinFunction(a = a)
  exIntensities(bf, d)
}

intensities_gamma <- function(d, a) {
  bf <- GammaBernsteinFunction(a = a)
  intensities(bf, d)
}

ex_intensities_pareto <- function(d, alpha, x0) {
  bf <- ParetoBernsteinFunction(alpha = alpha, x0 = x0)
  exIntensities(bf, d)
}

intensities_pareto <- function(d, alpha, x0) {
  bf <- ParetoBernsteinFunction(alpha = alpha, x0 = x0)
  intensities(bf, d)
}

ex_intensities_inverse_gaussian <- function(d, eta) { # nolint
  bf <- InverseGaussianBernsteinFunction(eta = eta)
  exIntensities(bf, d)
}

intensities_inverse_gaussian <- function(d, eta) {
  bf <- InverseGaussianBernsteinFunction(eta = eta)
  intensities(bf, d)
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
