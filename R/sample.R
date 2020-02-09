## #### Sample from extMO distributions ####
##

#' Sample with LFM and CPP subordinator
#'
#' Draws `n` independent samples from a `d`-variate extendible Marshall-Olkin
#' distribution corresponding to a LFM with a compound Poisson subordinator with
#' parameters `rate`, `rate_killing`, `rate_drift`, `rjump_name`, and
#' `rjump_arg_list`.
#'
#' - `rate` is the *jump intensity* of the compound Poisson subordinator:
#' at each given point-in-time, the waiting time to the next jump is
#' exponentially distributed with rate `rate`.
#' - `rate_killing` is the *killing intensity* of the compound Poisson
#' subordinator: the probability that the compound Poisson subordinator jumps to
#' its graveyard-state \eqn{\infty} between \eqn{t} and \eqn{s} is \eqn{1 -
#' \exp{\{-{ratekilling} (s-t)\}}}{1-exp{-rate_killing * (s-t)}}.
#'
#' @param n number of samples
#' @param d dimension
#' @param rate rate of CPP subordinator
#' @param rate_killing killing rate of CPP subordinator
#' @param rate_drift drift rate of CPP subordinator
#' @param rjump_name name of jump sampling function for jumps of CPP
#' subordinator
#' @param rjump_arg_list list with named arguments for jump sampling function
#' for jumps of CPP subordinator
#'
#' @return `rmo_lfm_cpp` implements the Lévy-frailty model representation with a
#' compound Poisson subordinator and returns an \eqn{n \times d}{n x d} numeric
#' matrix with the rows corresponding to independent and identically
#' distributed samples of the corresponding `d`-variate extendible
#' Marshall-Olkin distribution.
#'
#' @section References: For more information on this algorithm, see J.-F. Mai,
#' M. Scherer, "Simulating Copulas", World Scientific (2017), pp. 140 psqq.
#'
#' @examples
#' rmo_lfm_cpp(10L, 2L, 0.5, 0.1, 0.2, "rposval", list("value"=1))
#' rmo_lfm_cpp(10L, 2L, 0.5, 0, 0, "rexp", list("rate"=2))
#' \dontrun{
#' rmo_lfm_cpp(10L, 2L, 0, 0, 1, "rposval", list("value"=1))  ## independence
#' rmo_lfm_cpp(10L, 2L, 0, 1, 0, "rposval", list("value"=1))  ## comonotone
#' }
#'
#' @family samplers
#'
#' @include assert.R
#' @importFrom assertthat assert_that is.count
#' @importFrom stats rexp
#'
#' @export
#' @name rmo_lfm_cpp
rmo_lfm_cpp <- function(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list = list()) {
  assert_that(is.count(n), is.count(d), is_nonnegative_number(rate),
    is_nonnegative_number(rate_killing), is_nonnegative_number(rate_drift),
    is_positive_number(rate + rate_killing + rate_drift),
    is_rjump_name(rjump_name), is_rjump_arg_list(rjump_name, rjump_arg_list))
  rjump <- get(rjump_name)

  Rcpp__rmo_lfm_cpp(n, d, rate, rate_killing, rate_drift, rjump, rjump_arg_list)
}



## #### Auxiliary samplers ####
##

#' A wrapper for `rexp`
#'
#' Wraps an `ifelse`-clause arround `rexp` with special treatment for the case
#' `rate=0`.
#'
#' @inheritParams stats::rexp
#'
#' @importFrom stats rexp
#'
#' @keywords internal
#' @noRd
rexp_if_rate_zero_then_infinity <- function(n, rate) { # nolint
  if (rate == 0) {
    return(rep(Inf, n))
  }

  rexp(n, rate)
}

#' A dummy sampling function for deterministic, positive values
#'
#' @param n number of samples
#' @param value value to sample
#'
#' @return A `n` elements numeric vector with `value` in each component
#'
#' @examples
#' rposval(10L)       ## rep(1, 10L)
#' rposval(10L, pi)   ## rep(pi, 10L)
#'
#' @family samplers
#'
#' @keywords internal
#' @noRd
rposval <- function(n, value=1) {
  rep(value, times=n)
}
