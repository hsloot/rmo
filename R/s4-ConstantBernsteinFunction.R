#' @include error.R
#' @include s4-BernsteinFunction.R
NULL

#' Class for (almost) constant Bernstein functions
#'
#' @slot constant The nonnegative *killing* parameter (i.e. \eqn{a}
#'   in the representation)
#'
#' @description
#' A *constant Bernstein function* is a Bernstein function with only a
#' constant part (for \eqn{x > 0}), i.e. \eqn{b = 0} and \eqn{\nu = 0}. In
#' particular,
#' \deqn{
#'   \psi(x) = a , x > 0
#' }
#'
#' @seealso [BernsteinFunction-class],
#'   [valueOf()]
#'
#' @export ConstantBernsteinFunction
ConstantBernsteinFunction <- setClass("ConstantBernsteinFunction", # nolint
  contains = "BernsteinFunction",
  slots = c(constant = "numeric")
)

#' @describeIn ConstantBernsteinFunction-class Constructor
#' @aliases initialize,ConstantBernsteinFunction-method
#' @aliases initialize,ConstantBernsteinFunction,ANY-method
#'
#' @inheritParams methods::initialize
#' @param constant Nonnegative number.
#'
#' @examples
#' ConstantBernsteinFunction()
#' ConstantBernsteinFunction(constant = 0.2)
setMethod(
  "initialize", "ConstantBernsteinFunction",
  function(.Object, constant) { # nolint
    if (!missing(constant)) {
      .Object@constant <- constant # nolint
      validObject(.Object)
    }

    invisible(.Object)
  }
)

#' @importFrom checkmate qtest
setValidity(
  "ConstantBernsteinFunction",
  function(object) {
    if (!qtest(object@constant, "N1[0,)")) {
      return(error_msg_domain("constant", "N1[0,)"))
    }

    invisible(TRUE)
  }
)

#' @describeIn ConstantBernsteinFunction-class Display the object.
#' @aliases show,ConstantBernsteinFunction-method
#'
#' @export
setMethod( # nocov start
  "show", "ConstantBernsteinFunction",
  function(object) {
    cat(sprintf("An object of class %s\n", classLabel(class(object))))
    if (isTRUE(validObject(object, test = TRUE))) {
      cat(sprintf("- constant: %s\n", format(object@constant)))
    } else {
      cat("\t (invalid or not initialized)\n")
    }

    invisible(NULL)
  }
) # nocov end

#' @describeIn ConstantBernsteinFunction-class
#'   Calculates the iterated differences of the Bernstein function,
#'   see [valueOf()]
#' @aliases valueOf,ConstantBernsteinFunction,ANY-method
#'
#' @inheritParams valueOf
#'
#' @importFrom checkmate qassert assert check_numeric check_complex
#' @export
setMethod(
  "valueOf", "ConstantBernsteinFunction",
  function(object, x, difference_order = 0L, n = 1L, k = 0L, cscale = 1, ...) { # nolint
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
      out <- ifelse(
        x == 0, 0, multiply_binomial_coefficient(object@constant, n, k)
      )
    } else {
      out <- ifelse(
        x == 0, multiply_binomial_coefficient(object@constant, n, k), 0
      )
    }

    out
  }
)
