## #### Sample from MO distributions ####
##


#' Sample from a Marshall--Olkin distribution
#'
#' @description
#' Draws `n` independent samples from a `d`-variate Marshall-Olkin distribution
#' with shock rates `intensities`.
#'
#' @param n Number of samples
#' @param d Dimension
#' @param intensities Marshall-Olkin intensity rates
#'
#' @details
#' __The shock intensities__:
#' - The shock `intensities` must be stored in a vector of length
#'    \eqn{2^d-1}.
#' - A shock intensity of zero corresponds to an almost surely infinite
#'    shock.
#' - We use a binary representation to map a non-empty subset \eqn{J}
#'    of \eqn{\{ 1, \ldots, d\}}{{1, \ldots, d}} to integers \eqn{j} of
#'    \eqn{1, \ldots, 2^d-1}.
#'    In particular, \eqn{i} is a component in the set \eqn{J} corresponding to
#'    the integer \eqn{j} if, and only if,
#'    \eqn{j = \sum_{k=0}^\infty a_k 2^k}{\sum a[k] * 2^k}
#'    and \eqn{a_{i-1} = 1}{a[i-1] = 1}.
#'
#' __The exogenous shock model__ simulates a Marshall--Olkin distributed random
#' vector via exponentially distributed shock times for all non-empty subsets,
#' see \insertCite{@see pp. 104 psqq. @Mai2017a}{rmo} and
#' \insertCite{Marshall1967a}{rmo}.
#'
#' @return `rmo_esm` implements the *exogenous shock model* representation and
#'   returns an \eqn{n \times d}{n x d} numeric matrix with the rows
#'   corresponding to independent and identically distributed samples of a
#'   \eqn{d} variate Marshall-Olkin distribution with parameters `intensities`.
#'
#' @family samplers
#'
#' @examples
#' rmo_esm(10L, 2L, c(0.4, 0.3, 0.2))
#' rmo_esm(10L, 2L, c(1, 1, 0))         ## independence
#' rmo_esm(10L, 2L, c(0, 0, 1))         ## comonotone
#'
#' @references
#'  \insertAllCited{}
#'
#' @include assert.R RcppExports.R
#' @importFrom stats rexp
#' @importFrom assertthat assert_that is.count
#'
#' @export
#' @name rmo_esm
rmo_esm <- function(n, d, intensities) {
  assert_that(is.count(n), is_mo_parameter(d, intensities))

  Rcpp__rmo_esm(n, d, intensities)
}


#' @rdname rmo_esm
#'
#' @details
#' __The Arnold model__ simulates a Marshall--Olkin distributed random variable
#' by simulating a marked homogeneous Poisson process and where the
#' inter-arrival times correspond to shock shock-arrival times and the
#' marks to the specific shocks, see \insertCite{@see Sec. 3.1.2 @Mai2017a}{rmo}
#' and \insertCite{Arnold1975a}{rmo}.
#'
#' @return `rmo_arnold` implements the *Arnold model* representation and returns
#'  an \eqn{n \times d}{n x d} numeric matrix with the rows corresponding to
#'  independent and identically distributed samples of a \eqn{d} variate
#'  Marshall-Olkin distribution with parameters `intensities`.
#'
#' @examples
#' rmo_arnold(10L, 2L, c(0.4, 0.3, 0.2))
#' rmo_arnold(10L, 2L, c(1, 1, 0))         ## independence
#' rmo_arnold(10L, 2L, c(0, 0, 1))         ## comonotone
#'
#' @family samplers
#'
#' @include assert.R RcppExports.R
#' @importFrom stats rexp
#' @importFrom assertthat assert_that is.count
#'
#' @export
rmo_arnold <- function(n, d, intensities) {
  assert_that(is.count(n), is_mo_parameter(d, intensities))

  Rcpp__rmo_arnold(n, d, intensities)
}



## #### Sample from exMO distribution ####
##

#' Sample from an exchangeable MO distribution
#'
#' Draws `n` independent samples from a `d` variate exchangeable Marshall-Olkin
#' distribution with shock rates `ex_intensities`.
#'
#' - The *exchangeable* shock intensities must be stored in a vector of length
#' \eqn{d}.
#' - The entry \eqn{{exintensities}_{i}}{ex_intensities[i]} is the
#' intensity of a shock corresponding to a set with \eqn{i} elements.
#'
#' @section References:
#' For more information on this algorithm, see J.-F. Mai, M. Scherer,
#' "Simulating Copulas", World Scientific (2017), pp. 122 psqq.
#'
#' @param n number of samples
#' @param d dimension
#' @param ex_intensities exchangeable Marshall-Olkin intensity rates
#'
#' @return `rmo_ex_arnold` implements the modified Arnold model for the
#' exchangeable subclass and returns an \eqn{n \times d}{n x d} numeric matrix
#' with the rows corresponding to independent and identically disctributed
#' samples of a \eqn{d} variate exchangeable Marshall-Olkin distribution with
#' exchangeable parameters `ex_intensities`.
#'
#' @family samplers
#'
#' @examples
#' rmo_ex_arnold(10, 2, c(0.4, 0.2))
#' rmo_ex_arnold(10, 2, c(1, 0))      ## independence
#' rmo_ex_arnold(10, 2, c(0, 1))      ## comonotone
#'
#' @include assert.R
#' @importFrom assertthat assert_that is.count
#'
#' @export
#' @name rmo_ex_arnold
rmo_ex_arnold <- function(n, d, ex_intensities) {
  assert_that(is.count(n), is_ex_mo_parameter(d, ex_intensities))

  Rcpp__rmo_ex_arnold(n, d, ex_intensities)
}

## #### Sample from extMO distributions ####
##


#' Sample from Cuadras-Auge distribution
#'
#' Draws `n` independent samples from a `d` variate Cuadras-Augé distribution
#' with parameters `alpha` and `beta`.
#'
#' - `alpha` is the shock intensity of shocks that affect only single
#' components.
#' - `beta` is the shock intensity of the global shock that affects all
#' components.
#'
#' @param n number of samples
#' @param d dimension
#' @param alpha rate of individual shocks
#' @param beta rate of global shock
#'
#' @return `rmo_esm_cuadras_auge` implements an optimized version of the
#' *exogenous shock model* representation for the Cuadras-Augé family and
#' returns an \eqn{n \times d}{n x d} array matrix with rows corresponding to
#' the independent samples of size \eqn{d}.
#'
#' @seealso \code{\link{rmo_esm}}
#' @family samplers
#'
#' @examples
#' rmo_esm_cuadras_auge(10L, 2L, 0.5, 0.2)
#' rmo_esm_cuadras_auge(10L, 2L, 0, 1)      ## comonotone
#' rmo_esm_cuadras_auge(10L, 2L, 1, 0)      ## independence
#'
#' @include assert.R
#' @importFrom assertthat assert_that is.count
#'
#' @export
#' @name rmo_esm_cuadras_auge
rmo_esm_cuadras_auge <- function(n, d, alpha, beta) {
  assert_that(is.count(n), is_cuadras_auge_parameter(d, alpha, beta))

  Rcpp__rmo_esm_cuadras_auge(n, d, alpha, beta)
}



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
#'
#' rmo_lfm_cpp(10L, 2L, 0, 0, 1, "rposval", list("value"=1))  ## independence
#' rmo_lfm_cpp(10L, 2L, 0, 1, 0, "rposval", list("value"=1))  ## comonotone
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
  assert_that(is.count(n),
    is_lfm_cpp_mo_parameter(d, rate, rate_killing, rate_drift,
      rjump_name, rjump_arg_list))
  rjump <- get(rjump_name)

  Rcpp__rmo_lfm_cpp(n, d, rate, rate_killing, rate_drift, rjump, rjump_arg_list)
}



## #### Auxiliary samplers ####
##

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
rexp_if_rate_zero_then_infinity <- function(n, rate) { # only used in tests # nolint
  if (0 == rate) {
    return(rep(Inf, n))
  }

  rexp(n, rate)
}
