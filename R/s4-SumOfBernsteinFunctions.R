#' @include s4-BernsteinFunction.R
NULL

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
#' @seealso [BernsteinFunction-class],
#'   [valueOf()]
#'
#' @export SumOfBernsteinFunctions
SumOfBernsteinFunctions <- setClass("SumOfBernsteinFunctions", # nolint
    contains = "BernsteinFunction",
    slots = c(first = "BernsteinFunction", second = "BernsteinFunction")
)

#' @describeIn SumOfBernsteinFunctions-class Constructor
#' @aliases initialize,SumOfBernsteinFunctions-method
#' @aliases initialize,SumOfBernsteinFunctions,ANY-method
#'
#' @inheritParams methods::initialize
#' @param first Derives from [BernsteinFunction-class].
#' @param second Derives from [BernsteinFunction-class].
#'
#' @examples
#' SumOfBernsteinFunctions()
#' first_bf <- LinearBernsteinFunction(scale = 0.2)
#' second_bf <- AlphaStableBernsteinFunction(alpha = 0.5)
#' SumOfBernsteinFunctions(first = first_bf, second = second_bf)
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

#' @describeIn SumOfBernsteinFunctions-class Display the object.
#' @aliases show,SumOfBernsteinFunctions-method
#'
#' @importFrom utils capture.output
#'
#' @export
setMethod(
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
)

#' @describeIn SumOfBernsteinFunctions-class
#'   Calculates the iterated differences of the Bernstein function, see [valueOf()]
#' @aliases valueOf,SumOfBernsteinFunctions,ANY-method
#'
#' @inheritParams valueOf
#'
#' @export
setMethod(
    "valueOf", "SumOfBernsteinFunctions",
    function(object, x, difference_order = 0L, n = 1L, k = 0L, cscale = 1, ...) {
        valueOf(object@first, x, difference_order, n, k, cscale, ...) +
            valueOf(object@second, x, difference_order, n, k, cscale, ...)
    }
)
