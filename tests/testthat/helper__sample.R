## File contains test functions for various simulation Marshall--Olkin
## simulation algorithms.
##
## Naming convention for the functions: test__rmo_<name>_<specialisation>_R
##
## #### Setup ####
##
if (!"assertthat" %in% .packages()) {
  library("assertthat", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
}

## #### Bivariate implementations ####
##

#' Bivariate exogenous shock model
#'
#' A simple implementation of the bivariate exogenous shock model
#' algorithm.
#'
#' @param n number of simulations
#' @param intensities shock model intensity rates
#'
#' @returns A \eqn{n \times 2} numeric array with the simulation
#' 	results.
#'
#' @importFrom stats rexp
#'
#' @keywords internal
#' @noRd
test__rmo_esm_bivariate_R <- function(n, d, intensities) { # nolint
  assertthat::assert_that(assertthat::is.count(n), assertthat::is.count(d),
    d == 2L, is_mo_parameter(intensities), length(intensities) == 2^d-1)

  out <- matrix(0, nrow = n, ncol = 2)
    for (i in 1:n) {
      E1 <- rexp_if_rate_zero_then_infinity(1, intensities[1]) # nolint
      E2 <- rexp_if_rate_zero_then_infinity(1, intensities[2]) # nolint
      E3 <- rexp_if_rate_zero_then_infinity(1, intensities[3]) # nolint
      out[i, ] <- pmin(c(E1, E2), E3)
    }

  out
}

#' Bivariate Arnold model
#'
#' A simple implementation of the bivariate Arnold
#' model algorithm.
#'
#' @param n number of simulations
#' @param intensities shock model intensity rates
#'
#' @returns A \eqn{n \times 2} numeric array with the simulation
#' 	results.
#'
#' @importFrom stats rexp
#'
#' @keywords internal
#' @noRd
test__rmo_arnold_bivariate_R <- function(n, d, intensities) { # nolint
  assertthat::assert_that(assertthat::is.count(n), assertthat::is.count(d),
    d == 2L, is_mo_parameter(intensities), length(intensities) == 2^d-1)

  total_intensity <- sum(intensities)
  transition_probabilities <-intensities / total_intensity

  out <- matrix(0, nrow=n, ncol=2)
  for (i in 1:n) {
  destroyed <- rep(FALSE, 2)
    while (!all(destroyed)) {
      epsilon <- rexp(1, total_intensity) # nolint
      affected <- sample.int(3, 1, replace=FALSE, prob = transition_probabilities) # nolint
      out[i, !destroyed] <- out[i, !destroyed] + epsilon
      if (affected == 1) {
        destroyed[1] <- TRUE
      } else if (affected == 2) {
        destroyed[2] <- TRUE
      } else {
        destroyed <- rep(TRUE, 2)
      }
    }
  }

  out
}


#' Bivariate exchangeable Arnold model
#'
#' A simple implementation of the modified bivariate Arnold
#' model algorithm for the exchangeable shubclass.
#'
#' @param n number of simulations
#' @param ex_intensities shock model intensity rates
#'
#' @returns A \eqn{n \times 2} numeric array with the simulation
#' 	results.
#'
#' @importFrom stats rexp
#'
#' @keywords internal
#' @noRd
test__rmo_ex_arnold_bivariate_R <- function(n, d, ex_intensities) { # nolint
  assertthat::assert_that(assertthat::is.count(n), assertthat::is.count(d),
    d == 2L, is_exmo_parameter(ex_intensities), length(ex_intensities) == d)

  total_intensity <- 2*ex_intensities[[1]] + ex_intensities[[2]]
  transition_probabilities <- c(2*ex_intensities[[1]], ex_intensities[[2]]) /
    total_intensity

  out <- matrix(0, nrow=n, ncol=2)
  for (i in 1:n) {
    epsilon <- rexp(1, total_intensity)
    num_affected <- sample.int(2, 1, replace = FALSE, prob = transition_probabilities)

    if (num_affected < 2) {
      out[i, 1] <- epsilon

      epsilon <- rexp(1, sum(ex_intensities))
      num_affected <- sample.int(1, 1, replace = FALSE) # dummy
      out[i, 2] <- out[i, 1] + epsilon

      perm <- sample.int(2, 2, replace=FALSE)
      out[i, ] <- out[i, perm]
    } else {
      out[i, ] <- epsilon
      perm <- sample.int(2, 2, replace=FALSE) # dummy
    }
  }

  out
}


## #### Altenative implementations ####

#' Alternative implementation of exchangeable Arnold model
#'
#' A simple implementation of the modified bivariate Arnold
#' model algorithm for the exchangeable shubclass.
#'
#' @param n number of simulations
#' @param d dimension of MO distribution
#' @param ex_intensities shock model intensity rates
#'
#' @returns A \eqn{n \times d} numeric array with the simulation
#' 	results.
#'
#' @importFrom stats rexp
#'
#' @keywords internal
#' @noRd
test__rmo_ex_arnold_alternative_R <- function(n, d, ex_intensities) { # nolint
  assertthat::assert_that(assertthat::is.count(n), assertthat::is.count(d),
    is_exmo_parameter(ex_intensities), length(ex_intensities) == d)

  ex_a <- vapply(0:(d-1), function(x) sum(vapply(0:(d-x-1), function(y) choose(d-x-1, y) * ex_intensities[[y+1]], FUN.VALUE=0.5)), FUN.VALUE=0.5) # nolint

  out <- matrix(0, nrow=n, ncol=5)

  for (i in 1:n) {
    ex_a_tmp <- ex_a
    d_tmp <- d
    while (d_tmp > 0) {
      ex_a_tmp <- ex_a_tmp[1:d_tmp]
      ex_intensities_tmp <- vapply(1:d_tmp, function(x) sum(vapply(0:(x-1), function(y) (-1)^(y) * choose(x - 1, y) * ex_a_tmp[[d_tmp - x + y + 1]], FUN.VALUE = 0.5)), FUN.VALUE = 0.5) # nolint

      transition_probabilities <- vapply(1:d_tmp, function(x) choose(d_tmp, x), FUN.VALUE = 0.5) *
        ex_intensities_tmp # intermediate result
      total_intensity <- sum(transition_probabilities)
      transition_probabilities <- transition_probabilities / total_intensity

      epsilon <- rexp(1, total_intensity)
      num_affected <- sample.int(d_tmp, 1, replace = TRUE, prob = transition_probabilities)

      out[i, (d-d_tmp+1):d] <- out[i, (d-d_tmp+1):d] + epsilon
      d_tmp <- d_tmp - num_affected
    }

    perm <- sample.int(d, d, replace = FALSE)
    out[i, ] <- out[i, perm]
  }

  out
}
