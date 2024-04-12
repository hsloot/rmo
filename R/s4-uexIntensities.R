#' Calculate exchangeable shock-arrival intensities
#'
#' Calculates (unscaled) *exchangeable shock-arrival intensities*, see [rmo()].
#'
#' @inheritParams valueOf
#' @param d A positive integer, larger than two, for the *dimension*.
#' @param ... pass-through parameter.
#'
#' @details
#' For a given Bernstein function, the exchangeable shock-arrival intensities
#' are defined as
#' \deqn{
#'   \lambda_{i}
#'     = {(-1)}^{i-1} \Delta^{i}{ \psi{(d-i)} } ,
#'       \quad 1 \leq i \leq d .
#' }
#' The calculation of the exchangeable shock-arrival intensities using this
#' formula is usually not numerically stable. Consequently, the various
#' alternative approaches are used dependent on the class of the Bernstein
#' function.
#'
#' @seealso [rmo()]
#'
#' @importFrom methods setGeneric
#' @family Bernstein function generics
#' @export
#' @examples
#' bf <- AlphaStableBernsteinFunction(alpha = 0.4)
#' uexIntensities(bf, 3)
setGeneric(
  "uexIntensities",
  function(object, d, cscale = 1, ...) {
    standardGeneric("uexIntensities")
  }
)
