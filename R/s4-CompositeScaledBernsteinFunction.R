#' @include error.R
#' @include s4-BernsteinFunction.R
NULL

#' Class for composite scaled Bernstein functions
#'
#' Bernstein functions are stable under composition, i.e. if \eqn{\psi} is
#' a Bernstein function and `c > 0`, then
#' \deqn{
#'   x \mapsto \psi(c x)
#' }
#' is also a Bernstein function.
#'
#' @slot cscale The scale of the inner linear Bernstein function of the
#'   composition.
#' @slot original The original Bernstein function.
#'
#' @seealso [BernsteinFunction-class],
#'   [valueOf()]
#'
#' @export CompositeScaledBernsteinFunction
CompositeScaledBernsteinFunction <- setClass("CompositeScaledBernsteinFunction", # nolint
    contains = "BernsteinFunction",
    slots = c(cscale = "numeric", original = "BernsteinFunction")
)

#' @describeIn CompositeScaledBernsteinFunction-class Constructor
#' @aliases initialize,CompositeScaledBernsteinFunction-method
#' @aliases initialize,CompositeScaledBernsteinFunction,ANY-method
#'
#' @inheritParams methods::initialize
#' @param cscale Positive number.
#' @param original Derives from [BernsteinFunction-class].
#'
#' @examples
#' CompositeScaledBernsteinFunction()
#' cscale <- 0.5
#' bf_original <- AlphaStableBernsteinFunction()
#' CompositeScaledBernsteinFunction(cscale = cscale, original = bf_original)
setMethod(
    "initialize", "CompositeScaledBernsteinFunction",
    function(.Object, cscale, original) { # nolint
        if (!(missing(cscale) || missing(original))) {
            .Object@cscale <- cscale # nolint
            .Object@original <- original # nolint
            validObject(.Object)
        }

        invisible(.Object)
    }
)

#' @importFrom checkmate qtest
setValidity(
    "CompositeScaledBernsteinFunction",
    function(object) {
        if (!qtest(object@cscale, "N1[0,)")) {
            return(error_msg_domain("cscale", "N1[0,)"))
        }

        invisible(TRUE)
    }
)

#' @describeIn CompositeScaledBernsteinFunction-class Display the object.
#' @aliases show,CompositeScaledBernsteinFunction-method
#'
#' @importFrom utils capture.output
#'
#' @export
setMethod(
    "show", "CompositeScaledBernsteinFunction",
    function(object) {
        cat(sprintf("An object of class %s\n", classLabel(class(object))))
        if (isTRUE(validObject(object, test = TRUE))) {
            cat(sprintf("- cscale: %s\n", format(object@cscale)))
            cat("- original:\n")
            writeLines(
                paste0("\t", capture.output(show(object@original)))
            )
        } else {
            cat("\t (invalid or not initialized)\n")
        }

        invisible(NULL)
    }
)

#' @describeIn CompositeScaledBernsteinFunction-class
#'   Calculates the iterated differences of the Bernstein function,
#'   see [valueOf()]
#' @aliases valueOf,CompositeScaledBernsteinFunction,ANY-method
#'
#' @inheritParams valueOf
#'
#' @export
setMethod(
    "valueOf", "CompositeScaledBernsteinFunction",
    function(object, x, difference_order = 0L, n = 1, k = 0, cscale = 1, ...) {
        valueOf(
            object@original, x,
            difference_order = difference_order,
            n = n, k = k,
            cscale = cscale * object@cscale,
            ...
        )
    }
)
