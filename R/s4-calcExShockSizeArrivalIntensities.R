#' Calculate exchangeable shock-size-arrival intensities
#'
#' Calculates *exchangeable shock-size-arrival intensities*, the
#' `theta` parameter for [rexmo()].
#'
#' @inheritParams uexIntensities
#'
#' @details
#' For a given Bernstein function, the exchangeable shock-size-arrival
#' intensities are defined as
#' \deqn{
#'   \eta_{i}
#'     = \binom{d}{i} {(-1)}^{i-1} \Delta^{i}{ \psi{(d-i)} } ,
#'       \quad 1 \leq i \leq d .
#' }
#' The calculation of the exchangeable shock-size-arrival intensities using this
#' formula is usually not numerically stable. Consequently, the various
#' alternative approaches are used dependent on the class of the Bernstein
#' function.
#'
#' @seealso [rexmo()]
#'
#' @importFrom methods setGeneric
#' @family Bernstein function generics
#' @export
#' @examples
#' bf <- AlphaStableBernsteinFunction(alpha = 0.4)
#' calcExShockSizeArrivalIntensities(bf, 3)
setGeneric(
  "calcExShockSizeArrivalIntensities",
  function(object, d, cscale = 1, ...) {
    standardGeneric("calcExShockSizeArrivalIntensities")
  }
)
