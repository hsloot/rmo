## #### Sample from MO distributions ####
##


#' Sample from a Marshall--Olkin distribution
#'
#' Draws `n` independent samples from a `d`-variate Marshall-Olkin distribution
#' with shock arrival `intensities`.
#'
#' @param n an integer for the number of samples.
#' @param d an integer for the dimension of the sample.
#' @param intensities a numeric vector with the Marshall-Olkin shock arrival intensities.
#' @param method a character vector indicating which sampling algorithm should be used.
#'   Use "AM" for the *Arnold model* and "ESM" for the *exogenous shock model*.
#'
#' @details
#' __Parametrisation__:
#' The Marshall--Olkin distribution has the survival function
#' \deqn{
#'   \bar{F}(t)
#'      = \exp{\left\{ - \sum_{I} \lambda_I \max_{i \in I} t_i \right\}} , \quad t > 0 ,
#' }
#' for *shock arrival intensities* \eqn{\lambda_I \geq 0},
#' \eqn{\emptyset \neq I \subseteq {\{ 1 , \ldots, d \}}}.
#'
#' __The shock arrival intensities__:
#' - The shock arrival `intensities` must be stored in a vector of length
#'    \eqn{2^d-1}.
#' - A shock arrival intensity of zero corresponds to an almost surely infinite
#'    shock arrival time.
#' - We use a binary representation to map a non-empty subset \eqn{J}
#'    of \eqn{\{ 1, \ldots, d\}}{{1, \ldots, d}} to integers \eqn{j} of
#'    \eqn{1, \ldots, 2^d-1}.
#'    In particular, \eqn{i} is a component in the set \eqn{J} corresponding to
#'    the integer \eqn{j} if, and only if,
#'    \eqn{j = \sum_{k=0}^\infty a_k 2^k}{\sum a[k] * 2^k}
#'    and \eqn{a_{i-1} = 1}{a[i-1] = 1}.
#'
#' The __exogenous shock model__ simulates a Marshall--Olkin distributed random
#' vector via exponentially distributed shock times for all non-empty subsets,
#' see \insertCite{@see pp. 104 psqq. @Mai2017a}{rmo} and
#' \insertCite{Marshall1967a}{rmo}.
#'
#' The __Arnold model__ simulates a Marshall--Olkin distributed random variable
#' by simulating a marked homogeneous Poisson process and where the
#' inter-arrival times correspond to shock shock-arrival times and the marks to
#' the specific shocks, see \insertCite{@see Sec. 3.1.2 @Mai2017a}{rmo} and
#' \insertCite{Arnold1975a}{rmo}.
#'
#' @return `rmo` returns an \eqn{n \times d}{n x d} numeric matrix with the rows
#'   corresponding to independent and identically distributed samples of a
#'   \eqn{d} variate Marshall-Olkin distribution with parameters `intensities`.
#'
#' @family sampling-algorithms
#'
#' @examples
#' rmo(10L, 2L, c(0.4, 0.3, 0.2))
#' rmo(10L, 2L, c(1, 1, 0))         ## independence
#' rmo(10L, 2L, c(0, 0, 1))         ## comonotone
#'
#' rmo(10L, 2L, c(0.4, 0.3, 0.2), method = "ESM")
#' rmo(10L, 2L, c(1, 1, 0), method = "ESM")         ## independence
#' rmo(10L, 2L, c(0, 0, 1), method = "ESM")         ## comonotone
#'
#' rmo(10L, 2L, c(0.4, 0.3, 0.2), method = "AM")
#' rmo(10L, 2L, c(1, 1, 0), method = "AM")         ## independence
#' rmo(10L, 2L, c(0, 0, 1), method = "AM")         ## comonotone
#'
#' @references
#'  \insertAllCited{}
#'
#' @export
rmo <- function(n, d, intensities, method = c("AM", "ESM")) {
  method <- match.arg(method)
  if ("ESM" == method) {
    Rcpp__rmo_esm(n, d, intensities)
  } else if ("AM" == method) {
    Rcpp__rmo_am(n, d, intensities)
  }
}


## #### Sample from an exchangeable Marshall--Olkin distribution ####
##

#' Sample from an exchangeable Marshall-Olkin distribution
#'
#' Draws `n` independent samples from a `d` variate exchangeable Marshall-Olkin
#' distribution with shock-size arrival rates `ex_intensities`.
#'
#' @section References:
#' For more information on a similar algorithm, see J.-F. Mai, M. Scherer,
#' "Simulating Copulas", World Scientific (2017), pp. 122 psqq.
#'
#' @param n an integer for the number of samples.
#' @param d an integer for the dimension of the sample.
#' @param ex_intensities a numeric vector with the exchangeable Marshall-Olkin exchangeable
#'   shock-size arrival intensities.
#' @param method a character vector indicating which sampling algorithm should be used.
#'   Use "MDCM" for the *Markovian death-counting model*, "AM" for the *Arnold model*,
#'   and "ESM" for the *exogenous shock model*.
#'
#' @return
#' `rexmo` implements the Markovian model for the death-counting process of the exchangeable
#' subclass and returns an  \eqn{n \times d}{n x d} numeric matrix with the rows corresponding to
#' independent and identically disctributed samples of a \eqn{d} variate exchangeable Marshall-Olkin
#' distribution with exchangeable shock-size arrival intensities `ex_intensities`.
#'
#' @details
#' __Parameterisation__:
#' The exchangeable Marshall--Olkin distribution has the property that \eqn{\lambda_I} only depends
#' on the cardinality of \eqn{I}, i.e., the *(unscaled) exchangeable shock arrival  intensities* are
#' defined for \eqn{I} with \eqn{i = \lvert I\rvert} by \eqn{\lambda_i = \lambda_I}, see [rmo()].
#' The *(scaled) exchangeable shock-size arrival intensities* are defined by
#' \eqn{\eta_i = \binom{d}{i} \lambda_i}.
#'
#' @family sampling-algorithms
#'
#' @examples
#' rexmo(10, 2, c(2 * 0.4, 0.2))
#' rexmo(10, 2, c(2, 0))          ## independence
#' rexmo(10, 2, c(0, 1))          ## comonotone
#'
#' rexmo(10, 2, c(2 * 0.4, 0.2), method = "AM")
#' rexmo(10, 2, c(2, 0), method = "AM")          ## independence
#' rexmo(10, 2, c(0, 1), method = "AM")          ## comonotone
#'
#' rexmo(10, 2, c(2 * 0.4, 0.2), method = "ESM")
#' rexmo(10, 2, c(2, 0), method = "ESM")          ## independence
#' rexmo(10, 2, c(0, 1), method = "ESM")          ## comonotone
#'
#' @export
rexmo <- function(n, d, ex_intensities, method = c("MDCM", "AM", "ESM")) {
  method <- match.arg(method)
  if ("MDCM" == method) {
    Rcpp__rexmo_mdcm(n, d, ex_intensities)
  } else if (method %in% c("AM", "ESM")) {
    intensities <- uexi2i(
      sapply(seq_along(ex_intensities), function(i) {
        divide_binomial_coefficient(ex_intensities[[i]], d, i)
      }))
    rmo(n, d, intensities, method = method)
  }
}

## #### Sample from extendible Marshall-Olkin distributions ####
##


#' Sample from the armageddon ESM distribution
#'
#' Draws `n` independent samples from a `d` variate armageddon ESM distribution
#' with parameters `alpha` and `beta`.
#'
#' @param n Number of samples
#' @param d Dimension
#' @param alpha Shock intensity of individual shocks
#' @param beta Shock intensity of global shock
#'
#' @return `rarmextmo_esm` implements an optimized version of the
#' *exogenous shock model* algorithm for the armageddon shock family and
#' returns an \eqn{n \times d}{n x d} array matrix with rows corresponding to
#' the independent samples of size \eqn{d}.
#'
#' @details
#' __Parameterisation__:
#' The armageddon ESM distribution is a special case of the exchangeable Marshall-Olkin
#' distribution where \eqn{\lambda_i = 0 \forall 1 < i < d}.
#' The individual shock rate is \eqn{\alpha = \lambda_1} and the global shock
#' rate is \eqn{\beta = \lambda_d}.
#'
#' @seealso [rmo()]
#' @family sampling-algorithms
#'
#' @examples
#' rarmextmo_esm(10L, 2L, 0.5, 0.2)
#' rarmextmo_esm(10L, 2L, 0, 1)      ## comonotone
#' rarmextmo_esm(10L, 2L, 1, 0)      ## independence
#'
#' @export
rarmextmo_esm <- function(n, d, alpha, beta) {
  Rcpp__rarmextmo_esm(n, d, alpha, beta)
}



#' Sample with Lévy-frailty model with compound Poisson subordinator
#'
#' Draws `n` independent samples from a `d`-variate extendible Marshall-Olkin
#' distribution corresponding to a LFM with a compound Poisson subordinator.
#'
#' @param n Number of samples
#' @param d Dimension
#' @param rate Jump intensity of CPP subordinator
#' @param rate_killing Killing intensity of CPP subordinator
#' @param rate_drift Drift of CPP subordinator
#' @param rjump_name Name of jump sampling function for jumps of CPP
#'   subordinator
#' @param rjump_arg_list A list with named arguments for jump sampling function
#'   for jumps of CPP subordinator
#'
#' @return `rextmo_lfm` implements the Lévy-frailty model algorithm with a
#' compound Poisson subordinator and returns an \eqn{n \times d}{n x d} numeric
#' matrix with the rows corresponding to independent and identically
#' distributed samples of the corresponding `d`-variate extendible
#' Marshall-Olkin distribution.
#'
#' @details
#' __Model__:
#' The default times are defined as first exit times of a (killed) compound Poisson
#' subordinator \eqn{\Lambda} with *killing intensity* \eqn{a}, *drift* \eqn{b},
#' *jump intensity* \eqn{\xi}, and *jump distribution* \eqn{X}.
#'
#' __Model parameters__:
#' - `rate` is the *jump intensity* of the compound Poisson subordinator.
#' - `rate_killing` is the *killing intensity* of the compound Poisson
#'   subordinator.
#' - `rate_drift` is the *drift* of the compound Poisson subordinator.
#' - `rjump_name` and `rjump_arg_list` are the jump distribution name and
#'   list with jump distribution parameters. Currently, `"rexp"`, `"rposval"`,
#'   and `"rpareto"` are possible.
#'
#' @section References: For more information on this algorithm, see J.-F. Mai,
#' M. Scherer, "Simulating Copulas", World Scientific (2017), pp. 140 psqq.
#'
#' @examples
#' rextmo_lfm(10L, 2L, 0.5, 0.1, 0.2, "rposval", list("value"=1))
#' rextmo_lfm(10L, 2L, 0.5, 0, 0, "rexp", list("rate"=2))
#' rextmo_lfm(10L, 2L, 0.5, 0, 0, "rpareto", list("alpha"=log2(2 - 0.5), x0 = 1e-4))
#'
#' rextmo_lfm(10L, 2L, 0, 0, 1, "rposval", list("value"=1))  ## independence
#' rextmo_lfm(10L, 2L, 0, 1, 0, "rposval", list("value"=1))  ## comonotone
#'
#' @family sampling-algorithms
#'
#' @export
rextmo_lfm <- function(n, d,
                        rate, rate_killing, rate_drift,
                        rjump_name, rjump_arg_list = list()) {
  Rcpp__rextmo_lfm(n, d,
                    rate, rate_killing, rate_drift, rjump_name, rjump_arg_list)
}
