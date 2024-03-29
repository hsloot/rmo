#' @include error.R
#' @include s4-BernsteinFunction.R
#' @include s4-LevyBernsteinFunction.R
#' @include s4-CompleteBernsteinFunction.R
NULL

#' Class for Gamma Bernstein functions
#'
#' @slot a Scale parameter for the Lévy measure.
#'
#' @description
#' The *Gamma Bernstein function*, is the Bernstein function of a
#' subordinator with a (scaled) Gamma distribution. The representation is for
#' \eqn{a > 0}
#' \deqn{
#'   \psi(x) = \log(1 + \frac{x}{a}), x > 0.
#' }
#'
#' @details
#' For this Bernstein function, the higher-order alternating iterated forward
#' differences are known in closed form but cannot be evaluated numerically
#' without the danger of loss of significance. But we can use numerical
#' integration (here: [stats::integrate()]) to approximate it with the
#' following representation:
#' \deqn{
#'   {(-1)}^{k-1} \Delta^{k} \psi(x)
#'     = \int_{0}^{\infty} e^{-ux} {(1 - e^{-u})}^{k}
#'       \frac{e^{-au}}{u} du, x>0, k>0.
#' }
#'
#' This Bernstein function is no. 26 in the list of complete Bernstein functions
#' in Chp. 16 of \insertCite{Schilling2012a}{rmo}.
#'
#' @references
#'   \insertAllCited{}
#'
#' @seealso [BernsteinFunction-class], [LevyBernsteinFunction-class],
#'   [CompleteBernsteinFunction-class], [valueOf()]
#'
#' @export GammaBernsteinFunction
GammaBernsteinFunction <- setClass("GammaBernsteinFunction", # nolint
  contains = "CompleteBernsteinFunction",
  slots = c(a = "numeric")
)

#' @describeIn GammaBernsteinFunction-class Constructor
#' @aliases initialize,GammaBernsteinFunction-method
#' @aliases initialize,GammaBernsteinFunction,ANY-method
#'
#' @inheritParams methods::initialize
#' @param a Positive number.
#'
#' @examples
#' GammaBernsteinFunction()
#' GammaBernsteinFunction(a = 2)
setMethod(
  "initialize", "GammaBernsteinFunction",
  function(.Object, a) { # nolint
    if (!missing(a)) {
      .Object@a <- a # nolint
      validObject(.Object)
    }

    invisible(.Object)
  }
)

#' @importFrom checkmate qtest
setValidity(
  "GammaBernsteinFunction",
  function(object) {
    if (!qtest(object@a, "N1(0,)")) {
      return(error_msg_domain("a", "N1(0,)"))
    }

    invisible(TRUE)
  }
)

#' @describeIn GammaBernsteinFunction-class Display the object.
#' @aliases show,GammaBernsteinFunction-method
#'
#' @export
setMethod( # nocov start
  "show", "GammaBernsteinFunction",
  function(object) {
    cat(sprintf("An object of class %s\n", classLabel(class(object))))
    if (isTRUE(validObject(object, test = TRUE))) {
      cat(sprintf("- a: %s\n", format(object@a)))
    } else {
      cat("\t (invalid or not initialized)\n")
    }

    invisible(NULL)
  }
) # nocov end

#' @describeIn GammaBernsteinFunction-class
#'   see [LevyBernsteinFunction-class]
#' @aliases levyDensity,GammaBernsteinFunction-method
#'
#' @inheritParams levyDensity
#'
#' @section Lévy density:
#' \deqn{
#'   \nu(du)
#'     = \frac{\operatorname{e}^{-a u}}{u}, \quad u > 0 .
#' }
#'
#' @export
setMethod(
  "levyDensity", "GammaBernsteinFunction",
  function(object) {
    structure(
      function(x) {
        exp(-object@a * x) / x
      },
      lower = 0, upper = Inf, type = "continuous"
    )
  }
)

#' @describeIn GammaBernsteinFunction-class
#'   see [CompleteBernsteinFunction-class]
#' @aliases stieltjesDensity,GammaBernsteinFunction-method
#'
#' @inheritParams levyDensity
#'
#' @section Stieltjes density:
#' \deqn{
#'   \sigma(du)
#'     = 1 / u du, u > a .
#' }
#'
setMethod(
  "stieltjesDensity", "GammaBernsteinFunction",
  function(object) {
    structure(
      function(x) {
        1 / x
      },
      lower = object@a, upper = Inf, type = "continuous"
    )
  }
)

#' @keywords internal
setMethod(
  "valueOf0", "GammaBernsteinFunction",
  function(object, x, cscale = 1, ...) {
    assert(
      combine = "or",
      check_numeric(x, min.len = 1L, any.missing = FALSE),
      check_complex(x, min.len = 1L, any.missing = FALSE)
    )
    qassert(Re(x), "N+[0,)")
    qassert(cscale, "N1(0,)")
    x <- x * cscale
    log(1 + x / object@a)
  }
)
