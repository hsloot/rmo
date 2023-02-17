#' @include sample-rmo.R sample-rexmo.R
NULL

#' Sample from extendible Marshall–Olkin distributions
#'
#' Draws `n` iid samples from a `d`-variate
#' *extendible Marshall–Olkin distribution* parametrized by Bernstein functions
#' `bf`, essentially wrapping [rexmo()] by generating suitable
#' *exchangeable shock-size arrival intensities*.
#'
#' @inheritParams rexmo
#' @param bf a [BernsteinFunction-class] with the *Bernstein function* of a
#'   *extendible Marshall–Olkin distribution*.
#'
#' @return `rextmo` returns a numeric matrix with `n` rows and `d` columns with
#' the rows corresponding to iid distributed samples of a `d`-variate
#' *extendible Marshall–Olkin distribution* with *Bernstein function* `bf`.
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
#' The relationship between *Bernstein functions* and
#' *exchangeable shock-size arrival intensities* of the
#' *exchangeable Marshall–Olkin distribution*, see [rexmo()], is as follows:
#' \deqn{
#'     \eta_{i}
#'         = \binom{d}{i} {(-1)}^{i-1} \Delta{ \psi{(d-i)} } ,
#'             \quad i \in {\{ 1 , \ldots , d \}} .
#' }
#'
#' ### Calculation of the shock-size arrival intensities
#'
#' The *exchangeable shock-size arrival intensities* of a constant Bernstein
#' function with triplet \eqn{(a, 0, 0)} are
#' \deqn{
#'   \eta_{i}
#'     = \binom{d}{i} a 1_{\{ i = d \}} ,
#'       \quad i \in {\{ 1 , \ldots , d \}} .
#' }
#'
#' The *exchangeable shock-size arrival intensities* of a linear Bernstein
#' function with triplet \eqn{(0, b, 0)} are
#' \deqn{
#'   \eta_{i}
#'     = \binom{d}{i} b 1_{\{ i = 1 \}} ,
#'       \quad i \in {\{ 1 , \ldots , d \}} .
#' }
#'
#' The *exchangeable shock-size arrival intensities* of a Bernstein function
#' with *Lévy triple* \eqn{(0, 0, \nu)} are
#' \deqn{
#'   \eta_{i}
#'     = \int_{0}^{\infty}
#'       \binom{d}{i} {(1 - e^{-u})}^{i} e^{- u {(d-i)}} \nu{(du)} ,
#'         \quad i \in {\{ 1, \ldots, d \}}
#' }
#'
#' The *exchangeable shock-size arrival intensities* of a complete Bernstein
#' function with *Stieltjes triple* \eqn{(0, 0, \sigma)} are
#' \deqn{
#'   \eta_{i}
#'     = \int_{0}^{\infty}
#'       \binom{d}{i} u B{(1 + i, d - i + u)} \sigma{(du)}  ,
#'         \quad i \in {\{ 1, \ldots, d \}}
#' }
#'
#' The *exchangeable shock-size arrival intensities* of convex combinations of
#' Bernstein functions are the corresponding convex combinations of their
#' individual *exchangeable shock-size arrival intensities* with the same
#' coefficients.
#'
#' See \insertCite{Sloot2022a}{rmo} for the background, and detailed notes on
#' numerical approximations of these integrals.
#'
#' @family sampling-algorithms
#'
#' @examples
#' rextmo(
#'     10, 3,
#'     AlphaStableBernsteinFunction(alpha = log2(2 - 0.5))
#' )
#' # independence
#' rextmo(
#'     10, 3,
#'     LinearBernsteinFunction(scale = 1)
#' )
#' # comonotone
#' rextmo(
#'     10, 3,
#'     ConstantBernsteinFunction(constant = 1)
#' )
#'
#' rextmo(
#'     10, 3,
#'     AlphaStableBernsteinFunction(alpha = log2(2 - 0.5)),
#'     method = "AM"
#' )
#' # independence
#' rextmo(
#'     10, 3,
#'     LinearBernsteinFunction(scale = 1),
#'     method = "AM"
#' )
#' # comonotone
#' rextmo(
#'     10, 3,
#'     ConstantBernsteinFunction(constant = 1),
#'     method = "AM"
#' )
#'
#' rextmo(
#'     10, 3,
#'     AlphaStableBernsteinFunction(alpha = log2(2 - 0.5)),
#'     method = "ESM"
#' )
#' # independence
#' rextmo(
#'     10, 3,
#'     LinearBernsteinFunction(scale = 1),
#'     method = "ESM"
#' )
#' # comonotone
#' rextmo(
#'     10, 3,
#'     ConstantBernsteinFunction(constant = 1),
#'     method = "ESM"
#' )
#'
#' @references
#'  \insertAllCited{}
#'
#' @importFrom checkmate qassert assert_choice
#'
#' @export
rextmo <- function(n, d, bf, method = c("MDCM", "AM", "ESM")) {
    method <- match.arg(method)
    qassert(n, "X1[0,)")
    qassert(d, "X1[2,)")
    assert_choice(method, c("MDCM", "AM", "ESM"))

    rexmo(n, d, exIntensities(bf, d), method = method)
}
