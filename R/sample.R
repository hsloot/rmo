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
#' @export
#' @name rmo_esm
rmo_esm <- function(n, d, intensities) {
  Rcpp__rmo_esm(n, d, intensities)
}


#' @rdname rmo_esm
#'
#' @details
#' __The Arnold model__ simulates a Marshall--Olkin distributed random variable
#' by simulating a marked homogeneous Poisson process and where the
#' inter-arrival times correspond to shock shock-arrival times and the marks to
#' the specific shocks, see \insertCite{@see Sec. 3.1.2 @Mai2017a}{rmo} and
#' \insertCite{Arnold1975a}{rmo}.
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
#' @export
rmo_arnold <- function(n, d, intensities) {
  Rcpp__rmo_arnold(n, d, intensities)
}



## #### Sample from exMO distribution ####
##

#' Sample from an exchangeable MO distribution
#'
#' Draws `n` independent samples from a `d` variate exchangeable Marshall-Olkin
#' distribution with (scaled) shock rates `ex_intensities`.
#'
#' - The *exchangeable* shock intensities must be stored in a vector of length
#' \eqn{d}.
#' - The entry \eqn{{exintensities}_{i}}{ex_intensities[i]} is the
#' intensity of a shock corresponding to a set with \eqn{i} elements multiplied
#' by the number of shocks of cardinality \eqn{i}, i.e. \eqn{\binom{d}{i}}.
#'
#' @section References:
#' For more information on this algorithm, see J.-F. Mai, M. Scherer,
#' "Simulating Copulas", World Scientific (2017), pp. 122 psqq.
#'
#' @param n number of samples
#' @param d dimension
#' @param ex_intensities (Scaled) exchangeable Marshall-Olkin intensity rates
#'
#' @return `rexmo_markovian` implements the Markovian model for the
#' exchangeable subclass and returns an \eqn{n \times d}{n x d} numeric matrix
#' with the rows corresponding to independent and identically disctributed
#' samples of a \eqn{d} variate exchangeable Marshall-Olkin distribution with
#' exchangeable parameters `ex_intensities`.
#'
#' @family samplers
#'
#' @examples
#' rexmo_markovian(10, 2, c(2 * 0.4, 0.2))
#' rexmo_markovian(10, 2, c(2, 0))      ## independence
#' rexmo_markovian(10, 2, c(0, 1))      ## comonotone
#'
#' @export
#' @name rexmo_markovian
rexmo_markovian <- function(n, d, ex_intensities) {
  Rcpp__rexmo_markovian(n, d, ex_intensities)
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
#' rcamo_esm(10L, 2L, 0.5, 0.2)
#' rcamo_esm(10L, 2L, 0, 1)      ## comonotone
#' rcamo_esm(10L, 2L, 1, 0)      ## independence
#'
#' @export
#' @name rmo_esm_cuadras_auge
rcamo_esm <- function(n, d, alpha, beta) {
  Rcpp__rcamo_esm(n, d, alpha, beta)
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
#' @return `rextmo_lfm` implements the Lévy-frailty model representation with a
#' compound Poisson subordinator and returns an \eqn{n \times d}{n x d} numeric
#' matrix with the rows corresponding to independent and identically
#' distributed samples of the corresponding `d`-variate extendible
#' Marshall-Olkin distribution.
#'
#' @section References: For more information on this algorithm, see J.-F. Mai,
#' M. Scherer, "Simulating Copulas", World Scientific (2017), pp. 140 psqq.
#'
#' @examples
#' rextmo_lfm(10L, 2L, 0.5, 0.1, 0.2, "rposval", list("value"=1))
#' rextmo_lfm(10L, 2L, 0.5, 0, 0, "rexp", list("rate"=2))
#'
#' rextmo_lfm(10L, 2L, 0, 0, 1, "rposval", list("value"=1))  ## independence
#' rextmo_lfm(10L, 2L, 0, 1, 0, "rposval", list("value"=1))  ## comonotone
#'
#' @family samplers
#'
#' @export
#' @name rextmo_lfm
rextmo_lfm <- function(n, d,
                        rate, rate_killing, rate_drift,
                        rjump_name, rjump_arg_list = list()) {
  Rcpp__rextmo_lfm(n, d,
                    rate, rate_killing, rate_drift, rjump_name, rjump_arg_list)
}
