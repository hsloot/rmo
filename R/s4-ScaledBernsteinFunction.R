#' @include error.R
#' @include s4-BernsteinFunction.R
NULL

#' Class for scaled Bernstein functions
#'
#' Bernstein functions are stable under (nonegative) scalar multiplication,
#' i.e. if \eqn{\psi} is a Bernstein function and \eqn{\lambda \geq 0} , then
#' \deqn{
#'   x \mapsto \lambda \psi(x), x > 0,
#' }
#' is also a Bernstein function.
#'
#' @slot scale The scalar factor with which the original Bernstein function
#'   is to be multiplied.
#' @slot original The original Bernstein function which is to be multiplied.
#'
#' @seealso [BernsteinFunction-class],
#'   [valueOf()]
#'
#' @export ScaledBernsteinFunction
ScaledBernsteinFunction <- setClass("ScaledBernsteinFunction", # nolint
  contains = "BernsteinFunction",
  slots = c(scale = "numeric", original = "BernsteinFunction")
)

#' @describeIn ScaledBernsteinFunction-class Constructor
#' @aliases initialize,ScaledBernsteinFunction-method
#' @aliases initialize,ScaledBernsteinFunction,ANY-method
#'
#' @inheritParams methods::initialize
#' @param scale Positive number.
#' @param original Derives from [BernsteinFunction-class].
#'
#' @examples
#' ScaledBernsteinFunction()
#' original_bf <- AlphaStableBernsteinFunction(alpha = 0.5)
#' ScaledBernsteinFunction(scale = 2, original = original_bf)
setMethod(
  "initialize", "ScaledBernsteinFunction",
  function(.Object, scale, original) { # nolint
    if (!(missing(scale) || missing(original))) {
      .Object@scale <- scale # nolint
      .Object@original <- original # nolint
      validObject(.Object)
    }

    invisible(.Object)
  }
)

#' @importFrom checkmate qtest
setValidity(
  "ScaledBernsteinFunction",
  function(object) {
    if (!qtest(object@scale, "N1[0,)")) {
      return(error_msg_domain("scale", "N1[0,)"))
    }

    invisible(TRUE)
  }
)

#' @describeIn ScaledBernsteinFunction-class Display the object.
#' @aliases show,ScaledBernsteinFunction-method
#'
#' @importFrom utils capture.output
#'
#' @export
setMethod( # nocov start
  "show", "ScaledBernsteinFunction",
  function(object) {
    cat(sprintf("An object of class %s\n", classLabel(class(object))))
    if (isTRUE(validObject(object, test = TRUE))) {
      cat(sprintf("- scale: %s\n", format(object@scale)))
      cat("- original:\n")
      writeLines(
        paste0("\t", capture.output(show(object@original)))
      )
    } else {
      cat("\t (invalid or not initialized)\n")
    }

    invisible(NULL)
  }
) # nocov end

#' @describeIn ScaledBernsteinFunction-class
#'   Calculates the iterated differences of the Bernstein function,
#'   see [valueOf()]
#' @aliases valueOf,ScaledBernsteinFunction,ANY-method
#'
#' @inheritParams valueOf
#'
#' @export
setMethod(
  "valueOf", "ScaledBernsteinFunction",
  function(object, x, difference_order = 0L, n = 1L, k = 0L, cscale = 1, ...) { # nolint
    object@scale *
      valueOf(object@original, x, difference_order, n, k, cscale, ...)
  }
)
