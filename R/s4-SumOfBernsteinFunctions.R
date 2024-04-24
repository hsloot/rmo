#' Class for sums of two Bernstein functions
#'
#' Bernstein functions are stable under addition, i.e. if \eqn{\psi_1} and
#' \eqn{\psi_2} are two Bernstein functions, then
#' \deqn{
#'   x \mapsto \psi_1(x) + \psi_2(x) , x>0,
#' }
#' is also a Bernstein function.
#'
#' @slot first The first summand (derived from [BernsteinFunction-class]).
#' @slot second The second summand (derived from [BernsteinFunction-class]).
#'
#' @seealso [calcIterativeDifference()], [calcShockArrivalIntensities()],
#'   [calcExShockArrivalIntensities()], [calcExShockSizeArrivalIntensities()],
#'   [calcMDCMGeneratorMatrix()], [rextmo()], [rpextmo()]
#'
#' @docType class
#' @name SumOfBernsteinFunctions-class
#' @rdname SumOfBernsteinFunctions-class
#' @include s4-BernsteinFunction.R
#' @family Bernstein function classes
#' @family Bernstein function transformer classes
#' @export SumOfBernsteinFunctions
#' @examples
#' # Create an object of class SumOfBernsteinFunctions
#' SumOfBernsteinFunctions()
#' SumOfBernsteinFunctions(
#'   first = LinearBernsteinFunction(scale = 0.2),
#'   second = AlphaStableBernsteinFunction(alpha = 0.5)
#' )
SumOfBernsteinFunctions <- setClass("SumOfBernsteinFunctions", # nolint
  contains = "BernsteinFunction",
  slots = c(first = "BernsteinFunction", second = "BernsteinFunction")
)

#' @rdname hidden_aliases
#'
#' @inheritParams methods::initialize
#' @param first Derives from [BernsteinFunction-class].
#' @param second Derives from [BernsteinFunction-class].
setMethod(
  "initialize", "SumOfBernsteinFunctions",
  function(.Object, first, second) { # nolint
    if (!(missing(first) || missing(second))) {
      .Object@first <- first # nolint
      .Object@second <- second # nolint
      validObject(.Object)
    }

    invisible(.Object)
  }
)

#' @rdname hidden_aliases
#'
#' @inheritParams methods::show
#'
#' @importFrom utils capture.output
#' @export
setMethod( # nocov start
  "show", "SumOfBernsteinFunctions",
  function(object) {
    cat(sprintf("An object of class %s\n", classLabel(class(object))))
    if (isTRUE(validObject(object, test = TRUE))) {
      cat("- first:\n")
      writeLines(
        paste0("\t", capture.output(show(object@first)))
      )
      cat("- second:\n")
      writeLines(
        paste0("\t", capture.output(show(object@second)))
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
  "valueOf0", "SumOfBernsteinFunctions",
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
  "calcIterativeDifference", "SumOfBernsteinFunctions",
  function(object, x, difference_order = 0L, n = 1L, k = 0L, cscale = 1, ...) { # nolint
    calcIterativeDifference(
      object@first, x, difference_order, n, k, cscale, ...
    ) +
      calcIterativeDifference(
        object@second, x, difference_order, n, k, cscale, ...
      )
  }
)
