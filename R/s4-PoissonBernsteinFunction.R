#' Class for Poisson Bernstein functions
#'
#' @slot lambda The (positive) arrival rate of the underlying Poisson process.
#' @slot eta The fixed (positive) jump size of the Poisson process.
#'
#' @description
#' The Poisson process with arrival-rate \eqn{\lambda} and fixed jump size
#' \eqn{\eta} is a Lévy subordinator corresponding to the Bernstein function
#' \deqn{
#'   \psi(x) = 1 - e^{-x\eta}, x>0.
#' }
#'
#' @details
#' For the Poisson Bernstein function, the higher-order alternating iterated
#' forward differences can be calculated in closed form:
#' \deqn{
#'   {(-1)}^{k-1} \Delta^k \psi(x) = e^{-u\eta} (1-e^{-\eta})^k, x>0, k>0.
#' }
#'
#' ### Lévy density
#' \deqn{
#'   \nu(du)
#'     = \lambda \delta_{\eta}(du), \quad u > 0 .
#' }
#'
#' @seealso [BernsteinFunction-class], [LevyBernsteinFunction-class]
#'   [valueOf()]
#'
#' @include s4-BernsteinFunction.R s4-LevyBernsteinFunction.R
#' @family Bernstein function classes
#' @family Levy Bernstein function classes
#' @family Bernstein function boundary classes
#' @export PoissonBernsteinFunction
#' @examples
#' PoissonBernsteinFunction()
#' PoissonBernsteinFunction(eta = 2)
PoissonBernsteinFunction <- setClass("PoissonBernsteinFunction", # nolint
  contains = "LevyBernsteinFunction",
  slots = c(eta = "numeric")
)

#' @rdname hidden_aliases
#'
#' @inheritParams methods::initialize
#' @param eta Positive number.
setMethod(
  "initialize", "PoissonBernsteinFunction",
  function(.Object, eta) { # nolint
    if (!missing(eta)) {
      .Object@eta <- eta # nolint
      validObject(.Object)
    }

    invisible(.Object)
  }
)

#' @include error.R
#' @importFrom checkmate qtest
setValidity(
  "PoissonBernsteinFunction",
  function(object) {
    if (!qtest(object@eta, "N1[0,)")) {
      return(error_msg_domain("eta", "N1[0,)"))
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
  "show", "PoissonBernsteinFunction",
  function(object) {
    cat(sprintf("An object of class %s\n", classLabel(class(object))))
    if (isTRUE(validObject(object, test = TRUE))) {
      cat(sprintf("- eta: %s\n", format(object@eta)))
    } else {
      cat("\t (invalid or not initialized)\n")
    }

    invisible(NULL)
  }
) # nocov end

#' @rdname hidden_aliases
#'
#' @inheritParams levyDensity
#'
#' @include s4-levyDensity.R
#' @export
setMethod(
  "levyDensity", "PoissonBernsteinFunction",
  function(object) {
    structure(
      data.frame(x = object@eta, y = 1),
      type = "discrete"
    )
  }
)

#' @importFrom checkmate qassert
#' @keywords internal
setMethod(
  "valueOf0", "PoissonBernsteinFunction",
  function(object, x, cscale = 1, ...) {
    assert(
      combine = "or",
      check_numeric(x, min.len = 1L, any.missing = FALSE),
      check_complex(x, min.len = 1L, any.missing = FALSE)
    )
    qassert(Re(x), "N+[0,)")
    qassert(cscale, "N1(0,)")
    x <- x * cscale
    1 - exp(-x * object@eta)
  }
)
