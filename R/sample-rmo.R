#' Sample from Marshall–Olkin distributions
#'
#' Draws `n` iid samples from a `d`-variate *Marshall–Olkin distribution*
#' parametrized by *shock-arrival intensities*.
#'
#' @param n an integer for the *number of samples*.
#' @param d an integer for the *dimension*.
#' @param intensities a numeric vector for the *shock-arrival intensities*.
#' @param method a string indicating which sampling algorithm should be used.
#'   Use "AM" for the *Arnold model* and "ESM" for the *exogenous shock model*.
#'   We recommend using the *ESM* for small dimensions only; the *AM* can be
#'   used up until dimension \eqn{30}.
#'
#' @return `rmo` returns a numeric matrix with `n` rows and `d` columns, with
#'   the rows corresponding to iid distributed samples of a `d`-variate
#'   *Marshall–Olkin distribution* with *shock-arrival intensities*
#'   `intensities`.
#'
#'
#' @details
#' The *Marshall–Olkin distribution* was introduced in
#' \insertCite{Marshall1967a}{rmo} and has the survival function
#' \deqn{
#'     \bar{F}{(t)}
#'         = \exp{\left\{ - \sum_{I} \lambda_I \max_{i \in I} t_i \right\}} ,
#'             \quad t = {(t_{1}, \ldots, t_{d})} > 0 ,
#' }
#' for *shock-arrival intensities* \eqn{\lambda_I \geq 0},
#' \eqn{\emptyset \neq I \subseteq {\{ 1 , \ldots, d \}}}.
#' They are called *shock-arrival intensities* as they correspond to the rates
#' of independent exponential random variables from the
#' *exogenous shock model (ESM)*, and a shock-arrival intensity
#' \eqn{\lambda_{I}} of shock \eqn{I} equal to zero implies that the the shock
#' \eqn{I} never arrives. We use the following binary representation to map a
#' subsets of \eqn{\{ 1, \ldots, d\}}{{1, \ldots, d}} to integers
#' \eqn{0, \ldots, 2^d-1}:
#' \deqn{
#'   I \equiv \sum_{k \in I}{ 2^{k-1} }
#' }
#'
#' ## Simulation algorithms
#'
#' ### Exogenous shock model
#' The *exogenous shock model (ESM)* simulates a Marshall–Olkin distributed
#' random vector via independent exponentially distributed shock times for all
#' non-empty subsets of components and defines each component as the minimum of
#' all shock times corresponding to a subset containing this component, see
#' \insertCite{@see pp. 104 psqq. @Mai2017a}{rmo} and
#' \insertCite{Marshall1967a}{rmo}.
#'
#' ### Arnold model
#' The *Arnold model (AM)* simulates a Marshall–Olkin distributed random vector
#' by simulating a marked homogeneous Poisson process with set-valued marks.
#' The process is stopped when all components are *hit* by a shock,
#' see \insertCite{@see Sec. 3.1.2 @Mai2017a}{rmo} and
#' \insertCite{Arnold1975a}{rmo}.
#'
#' @family sampling-algorithms
#'
#' @examples
#' rmo(
#'     10, 3,
#'     c(0.4, 0.4, 0.1, 0.4, 0.1, 0.1, 0.4)
#' )
#' ## independence
#' rmo(
#'     10, 3,
#'     c(1, 1, 0, 1, 0, 0, 0)
#' )
#' ## comonotone
#' rmo(
#'     10, 3,
#'     c(0, 0, 0, 0, 0, 0, 1)
#' )
#'
#' rmo(
#'     10, 3,
#'     c(0.4, 0.4, 0.1, 0.4, 0.1, 0.1, 0.4),
#'     method = "ESM"
#' )
#' ## independence
#' rmo(
#'     10, 3,
#'     c(1, 1, 0, 1, 0, 0, 0),
#'     method = "ESM"
#' )
#' ## comonotone
#' rmo(
#'     10, 3,
#'     c(0, 0, 0, 0, 0, 0, 1),
#'     method = "ESM"
#' )
#'
#' rmo(
#'     10, 3,
#'     c(0.4, 0.4, 0.1, 0.4, 0.1, 0.1, 0.4),
#'     method = "AM"
#' )
#' ## independence
#' rmo(
#'     10, 3,
#'     c(1, 1, 0, 1, 0, 0, 0),
#'     method = "AM"
#' )
#' ## comonotone
#' rmo(
#'     10, 3,
#'     c(0, 0, 0, 0, 0, 0, 1),
#'     method = "AM"
#' )
#'
#' @references
#'  \insertAllCited{}
#'
#' @importFrom checkmate qassert assert_choice
#'
#' @export
rmo <- function(n, d, intensities, method = c("AM", "ESM")) {
    method <- match.arg(method)
    qassert(n, "X1[0,)")
    qassert(d, "X1[2,)")
    assert_choice(method, c("AM", "ESM"))

    if (method == "ESM") {
        Rcpp__rmo_esm(n, d, intensities)
    } else if (method == "AM") {
        Rcpp__rmo_am(n, d, intensities)
    } else {
        stop(sprintf("Method %s not implemented", method)) # nocov
    }
}
