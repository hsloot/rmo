#' Class for scaled Bernstein functions
#'
#' Bernstein functions are stable under (nonnegative) scalar multiplication,
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
#' @seealso [calcIterativeDifference()], [calcShockArrivalIntensities()],
#'   [calcExShockArrivalIntensities()], [calcExShockSizeArrivalIntensities()],
#'   [calcMDCMGeneratorMatrix()], [rextmo()], [rpextmo()]
#'
#' @docType class
#' @name ScaledBernsteinFunction-class
#' @rdname ScaledBernsteinFunction-class
#' @include s4-BernsteinFunction.R
#' @family Bernstein function classes
#' @family Bernstein function transformer classes
#' @export ScaledBernsteinFunction
#' @examples
#' # Create an object of class ScaleBernsteinFunction
#' ScaledBernsteinFunction()
#' ScaledBernsteinFunction(
#'   scale = 2,
#'   original = AlphaStableBernsteinFunction(alpha = 0.5)
#' )
ScaledBernsteinFunction <- setClass("ScaledBernsteinFunction", # nolint
  contains = "BernsteinFunction",
  slots = c(scale = "numeric", original = "BernsteinFunction")
)

#' @rdname hidden_aliases
#'
#' @inheritParams methods::initialize
#' @param scale Positive number.
#' @param original Derives from [BernsteinFunction-class].
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

#' @include error.R
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

#' @rdname hidden_aliases
#'
#' @inheritParams methods::show
#'
#' @importFrom utils capture.output
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

#' @rdname hidden_aliases
#'
#' @inheritParams valueOf0
#'
#' @include s4-valueOf0.R
#' @export
setMethod(
  "valueOf0", "ScaledBernsteinFunction",
  function(object, x, cscale = 1, ...) {
    calcIterativeDifference(object, x, cscale = cscale)
  }
)

#' @rdname hidden_aliases
#'
#' @inheritParams calcIterativeDifference
#'
#' @include s4-calcIterativeDifference.R
#' @export
setMethod(
  "calcIterativeDifference", "ScaledBernsteinFunction",
  function(object, x, difference_order = 0L, n = 1L, k = 0L, cscale = 1, ...) { # nolint
    object@scale *
      calcIterativeDifference(
        object@original, x, difference_order, n, k, cscale, ...
      )
  }
)
