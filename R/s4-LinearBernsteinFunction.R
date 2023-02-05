#' @include error.R s4-BernsteinFunction.R
NULL

#' Class for linear Bernstein functions
#'
#' @slot scale The non-negative \emph{drift} parameter
#'   (i.e. \eqn{b} in the representation)
#'
#' @description
#' \emph{A linear Bernstein function} is a Bernstein function with only a drift,
#' i.e. \eqn{a = 0} and \eqn{\nu = 0}. In particular,
#' \deqn{
#'  \psi(x) = b x, x > 0.
#' }
#'
#' @seealso [BernsteinFunction-class],
#'   [valueOf()]
#'
#' @export LinearBernsteinFunction
LinearBernsteinFunction <- setClass("LinearBernsteinFunction", # nolint
    contains = "BernsteinFunction",
    slots = c(scale = "numeric")
)

#' @describeIn LinearBernsteinFunction-class Constructor
#' @aliases initialize,LinearBernsteinFunction-method
#' @aliases initialize,LinearBernsteinFunction,ANY-method
#'
#' @inheritParams methods::initialize
#' @param scale Non-negative number.
#'
#' @examples
#' LinearBernsteinFunction()
#' LinearBernsteinFunction(scale = 2)
setMethod(
    "initialize", "LinearBernsteinFunction",
    function(.Object, scale) { # nolint
        if (!missing(scale)) {
            .Object@scale <- scale
            validObject(.Object)
        }

        invisible(.Object)
    }
)

#' @importFrom checkmate qtest
setValidity(
    "LinearBernsteinFunction",
    function(object) {
        if (!qtest(object@scale, "N1[0,)")) {
            return(error_msg_domain("scale", "N1[0,)"))
        }

        invisible(TRUE)
    }
)

#' @describeIn LinearBernsteinFunction-class Display the object.
#' @aliases show,LinearBernsteinFunction-method
#'
#' @export
setMethod(
    "show", "LinearBernsteinFunction",
    function(object) {
        cat(sprintf("An object of class %s\n", classLabel(class(object))))
        if (isTRUE(validObject(object, test = TRUE))) {
            cat(sprintf("- scale: %s\n", format(object@scale)))
        } else {
            cat("\t (invalid or not initialized)\n")
        }

        invisible(NULL)
    }
)

#' @describeIn LinearBernsteinFunction-class
#'   Calculates the iterated differences of the Bernstein function, see [valueOf()]
#' @aliases valueOf,LinearBernsteinFunction,ANY-method
#'
#' @inheritParams valueOf
#'
#' @importFrom checkmate qassert assert check_numeric check_complex
#' @export
setMethod(
    "valueOf", "LinearBernsteinFunction",
    function(object, x, difference_order = 0L, n = 1L, k = 0L, cscale = 1, ...) {
        assert(
            combine = "or",
            check_numeric(x, lower = 0, min.len = 1L, any.missing = FALSE),
            check_complex(x, min.len = 1L, any.missing = FALSE)
        )
        qassert(difference_order, "X1[0,)")
        qassert(cscale, "N1(0,)")
        qassert(n, "X1(0,)")
        qassert(k, "N1[0,)")

        if (0L == difference_order) {
            out <- multiply_binomial_coefficient((object@scale * cscale) * x, n, k)
        } else if (1L == difference_order) {
            out <- rep(multiply_binomial_coefficient((object@scale * cscale), n, k), length(x))
        } else {
            out <- rep(0, length(x))
        }

        out
    }
)
