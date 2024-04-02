#' @include sample-rmo.R
NULL

#' Simulate from exchangeable Marshall–Olkin distributions
#'
#' Draws `n` iid samples from a `d`-variate
#' *exchangeable Marshall–Olkin distribution* parametrized by exchangeable
#' *shock-size arrival intensities*.
#'
#' @inheritParams rmo
#' @param ex_intensities A numeric vector with the exchangeable
#' *shock-size arrival intensities*.
#' @param method A string indicating which sampling algorithm should be used.
#'   Use "MDCM" for the *Markovian death-counting model*, "AM" for the
#'   *Arnold model*, and "ESM" for the *exogenous shock model*. We recommend
#'   using the *ESM* only for small dimensions; the *AM* can be used up until
#'   dimension \eqn{30}.
#'
#' @return
#' `rexmo` returns a numeric matrix of size `n` x `d`. Each row corresponds to
#' an independently and identically (iid) distributed sample of a `d`-variate
#' *exchangeable Marshall–Olkin distribution* with specified parameters.
#'
#' @details
#' The *exchangeable Marshall–Olkin distribution* has the survival function
#' \deqn{
#'     \bar{F}{(t)}
#'         = \exp{\left\{
#'             -\sum_{i=1}^{d}{
#'                 {\left[
#'                     \sum_{j=0}^{d-i}{ \binom{d-i}{j} \lambda_{j+1} }
#'                 \right]}
#'                 \tau_{[i]}
#'             }
#'         \right\}} ,
#'           \quad t = {(t_{1}, \ldots, t_{d})} > 0 ,
#' }
#' for *exchangeable shock arrival intensities*
#' \eqn{\lambda_{i} \geq 0}, \eqn{1 \leq i \leq d}
#' and \eqn{t_{[1]} \geq \cdots \geq t_{[d]}}, see \insertCite{Mai2017a}{rmo}.
#'
#' The relationship of *exchangeable shock-size arrival intensities* to the
#' *shock-arrival intensities* of the *Marshall–Olkin distribution* is given by:
#' \deqn{
#'     \eta_{i}
#'         = \binom{d}{i} \lambda_{i} ,
#'           \quad i \in {\{1, \ldots, n\}} .
#' }
#'
#' ## Simulation algorithms
#'
#' ### Markovian death-counting model
#'
#' The Markovian death-counting model is a simulation algorithm used to generate
#' samples from the exchangeable Marshall-Olkin distribution. It simulates the
#' death-counting process of the random vector, which is a Markov process, until
#' all components are "dead". This process defines an order statistic that is
#' then used to obtain a sample through a random permutation. For more details
#' on this algorithm, refer to \insertCite{Sloot2022a}{rmo}.
#'
#' ### General Marshall–Olkin sampling algorithms
#'
#' The *exogenous shock model (ESM)* and *Arnold model (AM)* simulation
#' algorithms can be used to generate samples from the general *Marshall–Olkin
#' distribution*. In these algorithms, the exchangeable *shock-size arrival
#' intensities* are converted to the corresponding *shock-arrival intensities*
#' and passed to the [rmo()] function.
#'
#' @family sampling-algorithms
#'
#' @examples
#' rexmo(
#'   10, 3,
#'   c(1.2, 0.3, 0.4)
#' )
#' ## independence
#' rexmo(
#'   10, 3,
#'   c(3, 0, 0)
#' )
#' ## comonotone
#' rexmo(
#'   10, 3,
#'   c(0, 0, 1)
#' )
#'
#' rexmo(
#'   10, 3,
#'   c(1.2, 0.3, 0.4),
#'   method = "MDCM"
#' )
#' ## independence
#' rexmo(
#'   10, 3,
#'   c(3, 0, 0),
#'   method = "MDCM"
#' )
#' ## comonotone
#' rexmo(
#'   10, 3,
#'   c(0, 0, 1),
#'   method = "MDCM"
#' )
#'
#' rexmo(
#'   10, 3,
#'   c(1.2, 0.3, 0.4),
#'   method = "AM"
#' )
#' ## independence
#' rexmo(
#'   10, 3,
#'   c(3, 0, 0),
#'   method = "AM"
#' )
#' ## comonotone
#' rexmo(
#'   10, 3,
#'   c(0, 0, 1),
#'   method = "AM"
#' )
#'
#' rexmo(
#'   10, 3,
#'   c(1.2, 0.3, 0.4),
#'   method = "ESM"
#' )
#' ## independence
#' rexmo(
#'   10, 3,
#'   c(3, 0, 0),
#'   method = "ESM"
#' )
#' ## comonotone
#' rexmo(
#'   10, 3,
#'   c(0, 0, 1),
#'   method = "ESM"
#' )
#'
#' @references
#'  \insertAllCited{}
#'
#' @importFrom checkmate qassert assert_choice
#'
#' @export
rexmo <- function(n, d, ex_intensities, method = c("MDCM", "AM", "ESM")) {
  method <- match.arg(method)
  qassert(n, "X1[0,)")
  qassert(d, "X1[2,)")
  assert_choice(method, c("MDCM", "AM", "ESM"))

  if (method == "MDCM") {
    Rcpp__rexmo_mdcm(n, d, ex_intensities)
  } else if (method %in% c("AM", "ESM")) {
    intensities <- uexi2i(
      sapply(seq_along(ex_intensities), function(i) {
        divide_binomial_coefficient(ex_intensities[[i]], d, i)
      })
    )
    rmo(n, d, intensities, method = method)
  } else {
    stop(sprintf("Method %s not implemented", method)) # nocov
  }
}
