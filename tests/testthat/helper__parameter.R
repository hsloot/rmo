# #### Auxiliary functions ####

#' @keywords internal
#' @noRd
bf2ex_intensities <- function(d, bf) {
	sapply(1:d, function(i) valueOf(bf, d-i, i))
}

#' @keywords internal
#' @noRd
ex_intensities2intensities <- function(ex_intensities) {
  d <- length(ex_intensities)
  intensities <- numeric(2^d-1)

  for (j in seq_along(intensities)) {
    count <- 0
    for (i in 1:d) {
      count <- count + Rcpp__is_within(i, j)
    }
    intensities[j] <- ex_intensities[count]
  }

  intensities
}

# #### Extremal Bernstein functions ####

#' @keywords internal
#' @noRd
ex_intensities_constant <- function(d, constant) {
  bf <- ConstantBernsteinFunction(constant=constant)
  bf2ex_intensities(d, bf)
}

#' @keywords internal
#' @noRd
intensities_constant <- function(d, constant) {
  ex_intensities2intensities(ex_intensities_constant(d, constant))
}


#' @keywords internal
#' @noRd
ex_intensities_linear <- function(d, scale) {
  bf <- LinearBernsteinFunction(scale=scale)
  bf2ex_intensities(d, bf)
}

#' @keywords internal
#' @noRd
intensities_linear <- function(d, scale) {
  ex_intensities2intensities(ex_intensities_linear(d, scale))
}


#' @keywords internal
#' @noRd
ex_intensities_poisson <- function(d, lambda, eta) {
  bf <- PoissonBernsteinFunction(lambda=lambda, eta=eta)
  bf2ex_intensities(d, bf)
}

#' @keywords internal
#' @noRd
intensities_poisson <- function(d, lambda, eta) {
  ex_intensities2intensities(ex_intensities_poisson(d, lambda, eta))
}



# #### Algebraic Bernstein functions ####

#' @keywords internal
#' @noRd
ex_intensities_alpha_stable <- function(d, alpha) {
  bf <- AlphaStableBernsteinFunction(alpha=alpha)
  bf2ex_intensities(d, bf)
}

#' @keywords internal
#' @noRd
intensities_alpha_stable <- function(d, alpha) {
  ex_intensities2intensities(ex_intensities_alpha_stable(d, alpha))
}


# #### Lograthmic Bernstein functions ####

#' @keywords internal
#' @noRd
ex_intensities_gamma <- function(d, a) {
  bf <- GammaBernsteinFunction(a=a)
  bf2ex_intensities(d, bf)
}

#' @keywords internal
#' @noRd
intensities_gamma <- function(d, a) {
  ex_intensities2intensities(ex_intensities_gamma(d, a))
}



# #### Hierarchical intensities ####

#' @keywords internal
#' @noRd
intensities_hierarchical <- function(d1, d2, lambda, eta, a, alpha) {
  ex_intensities_0 <- ex_intensities_poisson(d1+d2, lambda, eta)
  ex_intensities_1 <- ex_intensities_gamma(d1, a)
  ex_intensities_2 <- ex_intensities_alpha_stable(d2, alpha)

  intensities <- ex_intensities2intensities(ex_intensities_0)
  for (j in seq_along(intensities)) {
    count_1 <- 0
    count_2 <- 0
    for (i in 1:d1) {
      count_1 <- count_1 + Rcpp__is_within(i, j)
    }
    for (i in 1:d2) {
      count_2 <- count_2 + Rcpp__is_within(d1+i, j)
    }

    if (count_1 > 0 && count_2 == 0) {
      intensities[j] <- intensities[j] + ex_intensities_1[count_1]
    } else if (count_2 > 0 && count_1 == 0) {
      intensities[j] <- intensities[j] + ex_intensities_2[count_2]
    }
  }

  intensities
}
