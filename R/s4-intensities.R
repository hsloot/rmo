#' Calculate the shock-arrival intensities
#'
#' Calculates the *shock-arrival intensities*, the `intensities` parameter for
#' [rmo()].
#'
#' @inheritParams uexIntensities
#'
#' @importFrom methods setGeneric
#' @family Bernstein function generics
#' @export
setGeneric(
  "intensities",
  function(object, d, cscale = 1, ...) {
    standardGeneric("intensities")
  }
)
