#' Evaluate Bernstein functions
#'
#' @description
#' Calculate the values for a Bernstein function function.
#'
#' @param object An object deriving from the class [BernsteinFunction-class].
#' @param x A nonnegative numeric vector at which the iterated difference of
#'   the Bernstein function is evaluated.
#' @param cscale A positive number for the *composite scaling factor*.
#' @param ... Pass-through parameter.
#'
#' @family Bernstein function generics
#' @importFrom methods setGeneric
#' @export
#' @examples
#' bf <- AlphaStableBernsteinFunction(alpha = 0.7)
#' calcValue(bf, 1:5)
setGeneric(
  "calcValue",
  function(object, x, cscale = 1, ...) {
    standardGeneric("calcValue")
  }
)
