#' Calculate exchangeable shock-size-arrival intensities
#'
#' Calculates *exchangeable shock-size-arrival intensities*, the
#' `ex_intensities` parameter for [rexmo()].
#'
#' @inheritParams uexIntensities
#'
#' @importFrom methods setGeneric
#' @family Bernstein function generics
#' @export
setGeneric(
  "exIntensities",
  function(object, d, cscale = 1, ...) {
    standardGeneric("exIntensities")
  }
)
