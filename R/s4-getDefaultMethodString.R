#' Default method for approximating Bernstein function differences
#'
#' @description
#' Evaluating Bernstein function differences is usually not numerically stable.
#' Consequently, various alternative approaches are used dependent on the class
#' of the Bernstein function. This method returns a String indicating the
#' default method used for the approximation.
#'
#' @param object An object deriving from the class [BernsteinFunction-class].
#'
#' @return
#' A String indicating the default method used for the approximation.
#'
#' @family Bernstein function generics
#' @importFrom methods setGeneric
#' @export
#' @examples
#' getDefaultMethodString(AlphaStableBernsteinFunction(alpha = 0.7))
setGeneric(
  "getDefaultMethodString",
  function(object) {
    standardGeneric("getDefaultMethodString")
  }
)
