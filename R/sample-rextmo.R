#' Simulate from extendible Marshall–Olkin distributions
#'
#' @description
#' Draws `n` iid samples from a `d`-variate *extendible Marshall–Olkin
#' distribution* parametrized by a Bernstein function `bf`.
#'
#' @inheritParams rexmo
#' @param bf A [BernsteinFunction-class] representing the *Bernstein function*
#'   of a *extendible Marshall–Olkin distribution*.
#'
#' @return
#' `rextmo` returns a numeric matrix of size `n` x `d` rows and `d`. Each row
#' corresponds to an independently and identically (iid) distributed sample from
#' a `d`-variate *extendible Marshall–Olkin distribution* with specified
#' parameters.
#'
#' @details
#' The *extendible Marshall–Olkin distribution* has the survival function
#' \deqn{
#'     \bar{F}{(t)}
#'         = \exp{\left\{
#'             - \sum_{i=1}^{d}{ {[ \psi{(i)} - \psi{(i-1)} ]} t_{[i]} }
#'           \right\}} ,
#'             \quad t = {(t_{1}, \ldots, t_{d})} > 0 ,
#' }
#' for *Bernstein functions* \eqn{\psi}, see [BernsteinFunction-class], and
#' \eqn{t_{[1]} \geq \cdots \geq t_{[d]}}, see \insertCite{Mai2017a}{rmo}.
#'
#' The relationship between *Bernstein functions* and
#' *exchangeable shock-size arrival intensities* of the
#' *exchangeable Marshall–Olkin distribution*, see [rexmo()], is as follows:
#' \deqn{
#'     \eta_{i}
#'         = \binom{d}{i} {(-1)}^{i-1} \Delta{ \psi{(d-i)} } ,
#'             \quad i \in {\{ 1 , \ldots , d \}} .
#' }
#'
#' This formula for the *exchangeable shock-size arrival intensities* is not
#' numerically stable in higher dimensions, and [rextmo()] uses
#' approximation techniques from \insertCite{Sloot2022a}{rmo} to calculate them.
#'
#' @references
#'  \insertAllCited{}
#'
#' @family sampling-algorithms
#'
#' @include sample-rmo.R sample-rexmo.R
#' @importFrom checkmate qassert assert_choice
#' @export
#' @examples
#' rextmo(
#'   10, 3,
#'   AlphaStableBernsteinFunction(alpha = log2(2 - 0.5))
#' )
#' # independence
#' rextmo(
#'   10, 3,
#'   LinearBernsteinFunction(scale = 1)
#' )
#' # comonotone
#' rextmo(
#'   10, 3,
#'   ConstantBernsteinFunction(constant = 1)
#' )
#'
#' rextmo(
#'   10, 3,
#'   AlphaStableBernsteinFunction(alpha = log2(2 - 0.5)),
#'   method = "AM"
#' )
#' # independence
#' rextmo(
#'   10, 3,
#'   LinearBernsteinFunction(scale = 1),
#'   method = "AM"
#' )
#' # comonotone
#' rextmo(
#'   10, 3,
#'   ConstantBernsteinFunction(constant = 1),
#'   method = "AM"
#' )
#'
#' rextmo(
#'   10, 3,
#'   AlphaStableBernsteinFunction(alpha = log2(2 - 0.5)),
#'   method = "ESM"
#' )
#' # independence
#' rextmo(
#'   10, 3,
#'   LinearBernsteinFunction(scale = 1),
#'   method = "ESM"
#' )
#' # comonotone
#' rextmo(
#'   10, 3,
#'   ConstantBernsteinFunction(constant = 1),
#'   method = "ESM"
#' )
rextmo <- function(n, d, bf, method = c("MDCM", "AM", "ESM")) {
  method <- match.arg(method)
  qassert(n, "X1[0,)")
  qassert(d, "X1[2,)")
  assert_choice(method, c("MDCM", "AM", "ESM"))

  rexmo(n, d, exIntensities(bf, d), method = method)
}
