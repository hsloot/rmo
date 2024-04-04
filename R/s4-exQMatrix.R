#' Calculate the MDCM Markovian generator matrix
#'
#' Calculates the *infinitesimal Markov generator matrix* of the corresponding
#' (Markovian) default-counting process, used internally by [rexmo()].
#'
#' @inheritParams uexIntensities
#'
#' @importFrom methods setGeneric
#' @family Bernstein function generics
#' @export
setGeneric(
  "exQMatrix",
  function(object, d, cscale = 1, ...) {
    standardGeneric("exQMatrix")
  }
)
