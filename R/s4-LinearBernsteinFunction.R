#' Class for linear Bernstein functions
#'
#' @slot scale The nonnegative *drift* parameter (i.e. \eqn{b} in the
#'   representation).
#'
#' @description
#' *A linear Bernstein function* is a Bernstein function with only a drift,
#' i.e., \eqn{a = 0} and \eqn{\nu = 0}. In particular,
#' \deqn{
#'  \psi(x) = b x, x > 0.
#' }
#'
#' @seealso [calcIterativeDifference()], [calcShockArrivalIntensities()],
#'   [calcExShockArrivalIntensities()], [calcExShockSizeArrivalIntensities()],
#'   [calcMDCMGeneratorMatrix()], [rextmo()], [rpextmo()]
#'
#' @docType class
#' @name LinearBernsteinFunction-class
#' @rdname LinearBernsteinFunction-class
#' @aliases LinearBernsteinFunction
#' @include s4-BernsteinFunction.R
#' @family Bernstein function classes
#' @family Bernstein function boundary classes
#' @export LinearBernsteinFunction
#' @examples
#' # Create an object of class LinearBernsteinFunction
#' LinearBernsteinFunction()
#' LinearBernsteinFunction(scale = 0.2)
#'
#' # Evaluate the Bernstein function
#' bf <- LinearBernsteinFunction(scale = 0.3)
#' calcIterativeDifference(bf, 1:5)
#'
#' # Calculate shock-arrival intensities
#' bf <- LinearBernsteinFunction(scale = 0.8)
#' calcShockArrivalIntensities(bf, 3)
#'
#' # Calculate exchangeable shock-arrival intensities
#' bf <- LinearBernsteinFunction(scale = 0.4)
#' calcExShockArrivalIntensities(bf, 3)
#'
#' # Calculate exchangeable shock-size arrival intensities
#' bf <- LinearBernsteinFunction(scale = 0.2)
#' calcExShockSizeArrivalIntensities(bf, 3)
#'
#' # Calculate the Markov generator
#' bf <- LinearBernsteinFunction(scale = 0.6)
#' calcMDCMGeneratorMatrix(bf, 3)
LinearBernsteinFunction <- setClass("LinearBernsteinFunction", # nolint
  contains = "BernsteinFunction",
  slots = c(scale = "numeric")
)

#' @rdname hidden_aliases
#'
#' @inheritParams methods::initialize
#' @param scale Nonnegative number.
setMethod(
  "initialize", "LinearBernsteinFunction",
  function(.Object, scale) { # nolint
    if (!missing(scale)) {
      .Object@scale <- scale # nolint
      validObject(.Object)
    }

    invisible(.Object)
  }
)

#' @include error.R
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

#' @rdname hidden_aliases
#'
#' @inheritParams methods::show
#'
#' @export
setMethod( # nocov start
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
) # nocov end

#' @rdname hidden_aliases
#'
#' @inheritParams calcValue
#'
#' @include s4-calcValue.R
#' @export
setMethod(
  "calcValue", "LinearBernsteinFunction",
  function(object, x, cscale = 1, ...) {
    calcIterativeDifference(object, x, cscale = cscale)
  }
)

#' @rdname hidden_aliases
#'
#' @inheritParams calcIterativeDifference
#'
#' @include s4-calcIterativeDifference.R RcppExports.R
#' @importFrom checkmate qassert assert check_numeric check_complex
#' @export
setMethod(
  "calcIterativeDifference", "LinearBernsteinFunction",
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
      out <- multiply_binomial_coefficient(
        (object@scale * cscale) * x, n, k
      )
    } else if (1L == difference_order) {
      out <- rep(
        multiply_binomial_coefficient((object@scale * cscale), n, k),
        length(x)
      )
    } else {
      out <- rep(0, length(x))
    }

    out
  }
)
