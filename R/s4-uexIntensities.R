#' Calculate exchangeable shock-arrival intensities
#'
#' Calculates (unscaled) *exchangeable shock-arrival intensities*, see [rmo()]
#' and [rexmo()].
#'
#' @inheritParams levyDensity
#' @param d a positive integer, larger than two, for the *dimension*.
#' @param cscale a positive number for the *composite scaling factor*.
#' @param ... pass-through parameter
#'
#' @importFrom methods setGeneric
#' @family Bernstein function generics
#' @export
setGeneric(
  "uexIntensities",
  function(object, d, cscale = 1, ...) {
    standardGeneric("uexIntensities")
  }
)
