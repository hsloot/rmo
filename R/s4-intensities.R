#' Calculate the shock-arrival intensities
#'
#' Calculates the *shock-arrival intensities*, the `intensities` parameter for
#' [rmo()].
#'
#' @inheritParams uexIntensities
#'
#' @details
#' For a given Bernstein function, the shock-arrival intensities are defined as
#' \deqn{
#'   \lambda_{I}
#'     = {(-1)}^{{\lvert I\rvert}-1}
#'       \Delta^{{\lvert I\rvert}}{ \psi{(d-{\lvert I\rvert})} } ,
#'         \quad 1 \leq {\lvert I\rvert} \leq d .
#' }
#' The calculation of the shock-arrival intensities using this formula is
#' usually not numerically stable. Consequently, the various alternative
#' approaches are used dependent on the class of the Bernstein function.
#'
#' The following binary representation is used to map subsets \eqn{I} of
#' \eqn{{\{1, \ldots, d\}}} to an integers \eqn{0, \ldots, 2^d-1}:
#' \deqn{
#'   I
#'     \equiv \sum_{k \in I}{ 2^{k-1} } .
#' }
#'
#' @seealso [rmo()]
#'
#' @importFrom methods setGeneric
#' @family Bernstein function generics
#' @export
#' @examples
#' bf <- AlphaStableBernsteinFunction(alpha = 0.7)
#' intensities(bf, 3)
setGeneric(
  "intensities",
  function(object, d, cscale = 1, ...) {
    standardGeneric("intensities")
  }
)
