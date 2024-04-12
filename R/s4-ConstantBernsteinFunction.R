#' Class for (almost) constant Bernstein functions
#'
#' @slot constant The nonnegative *killing* parameter (i.e., \eqn{a} in the
#'   representation)
#'
#' @description
#' A *constant Bernstein function* is a Bernstein function with only a constant
#' part (for \eqn{x > 0}), i.e., \eqn{b = 0} and \eqn{\nu = 0}. In
#' particular,
#' \deqn{
#'   \psi(x) = a , x > 0
#' }
#'
#' @seealso [valueOf()], [intensities()], [uexIntensities()], [exIntensities()],
#'   [exQMatrix()], [rextmo()], [rpextmo()]
#'
#' @docType class
#' @name ConstantBernsteinFunction-class
#' @rdname ConstantBernsteinFunction-class
#' @aliases ConstantBernsteinFunction
#' @include s4-BernsteinFunction.R
#' @family Bernstein function classes
#' @family Bernstein function boundary classes
#' @export ConstantBernsteinFunction
#' @examples
#' # Create an object of class ConstantBernsteinFunction
#' ConstantBernsteinFunction()
#' ConstantBernsteinFunction(constant = 0.2)
#'
#' # Evaluate the Bernstein function
#' bf <- ConstantBernsteinFunction(constant = 0.3)
#' valueOf(bf, 1:5)
#'
#' # Calculate shock-arrival intensities
#' bf <- ConstantBernsteinFunction(constant = 0.8)
#' intensities(bf, 3)
#'
#' # Calculate exchangeable shock-arrival intensities
#' bf <- ConstantBernsteinFunction(constant = 0.4)
#' uexIntensities(bf, 3)
#'
#' # Calculate exchangeable shock-size arrival intensities
#' bf <- ConstantBernsteinFunction(constant = 0.2)
#' exIntensities(bf, 3)
#'
#' # Calculate the Markov generator
#' bf <- ConstantBernsteinFunction(constant = 0.6)
#' exQMatrix(bf, 3)
ConstantBernsteinFunction <- setClass("ConstantBernsteinFunction", # nolint
  contains = "BernsteinFunction",
  slots = c(constant = "numeric")
)

#'@rdname hidden_aliases
#'
#' @inheritParams methods::initialize
#' @param constant Nonnegative number.
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

#' @include error.R
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

#' @rdname hidden_aliases
#'
#' @inheritParams methods::show
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

#' @rdname hidden_aliases
#'
#' @inheritParams valueOf
#'
#' @include s4-valueOf.R RcppExports.R
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
