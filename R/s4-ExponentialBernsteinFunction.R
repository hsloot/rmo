#' Class for Exponential Bernstein functions
#'
#' @slot lambda The index \eqn{\lambda}.
#'
#' @description
#' For the Exponential jump CPP subordinator with \eqn{\lambda > 0},
#' the corresponding Bernstein function is
#' \deqn{
#'   \psi(x) = \frac{x}{x + \lambda}, x>0.
#' }
#'
#' @details
#' For the Exponential jump CPP Bernstein function, the higher order
#' alternating iterated forward differences are known in closed form:
#' \deqn{
#'   {(-1)}^{k-1} \Delta^k \psi(x)
#'    = \lambda \cdot B(k+1, x+\lambda), x>0, k>0 .
#' }
#'
#' This Bernstein function is no. 4 in the list of complete Bernstein functions
#' in Chp. 16 of \insertCite{Schilling2012a}{rmo}.
#'
#' ### Lévy density
#' \deqn{
#'   \nu(du)
#'     = \lambda \operatorname{e}^{-\lambda u}, \quad u > 0 .
#' }
#' ### Stieltjes density
#' \deqn{
#'   \sigma(du)
#'     = \delta_{\lambda}(du), \quad u > 0 .
#' }
#'
#' @references
#'   \insertAllCited{}
#'
#' @seealso [BernsteinFunction-class], [LevyBernsteinFunction-class],
#'   [CompleteBernsteinFunction-class], [valueOf()]
#'
#' @include s4-BernsteinFunction.R s4-CompleteBernsteinFunction.R
#' @family Bernstein function classes
#' @family Levy Bernstein function classes
#' @family Complete Bernstein function classes
#' @family Algebraic Bernstein function classes
#' @export ExponentialBernsteinFunction
#' @examples
#' ExponentialBernsteinFunction()
#' ExponentialBernsteinFunction(lambda = 0.5)
ExponentialBernsteinFunction <- setClass("ExponentialBernsteinFunction", # nolint
  contains = "CompleteBernsteinFunction",
  slots = c("lambda" = "numeric")
)

#' @rdname hidden_aliases
#'
#' @inheritParams methods::initialize
#' @param lambda Positive number.
setMethod(
  "initialize", "ExponentialBernsteinFunction",
  function(.Object, lambda) { # nolint
    if (!missing(lambda)) {
      .Object@lambda <- lambda # nolint
      validObject(.Object)
    }

    invisible(.Object)
  }
)

#' @include error.R
#' @importFrom checkmate qtest
setValidity(
  "ExponentialBernsteinFunction",
  function(object) {
    if (!qtest(object@lambda, "N1(0,)")) {
      return(error_msg_domain("lambda", "N1(0,)"))
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
  "show", "ExponentialBernsteinFunction",
  function(object) {
    cat(sprintf("An object of class %s\n", classLabel(class(object))))
    if (isTRUE(validObject(object, test = TRUE))) {
      cat(sprintf("- lambda: %s\n", format(object@lambda)))
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
  "levyDensity", "ExponentialBernsteinFunction",
  function(object) {
    structure(
      function(x) {
        object@lambda * exp(-object@lambda * x)
      },
      lower = 0, upper = Inf, type = "continuous"
    )
  }
)

#' @rdname hidden_aliases
#'
#' @inheritParams levyDensity
#'
#' @include s4-stieltjesDensity.R
#' @export
setMethod(
  "stieltjesDensity", "ExponentialBernsteinFunction",
  function(object) {
    structure(
      data.frame(x = object@lambda, y = 1),
      type = "discrete"
    )
  }
)

#' @keywords internal
setMethod(
  "defaultMethod", "ExponentialBernsteinFunction",
  function(object) {
    "stieltjes"
  }
)

#' @keywords internal
setMethod(
  "valueOf0", "ExponentialBernsteinFunction",
  function(object, x, cscale = 1, ...) {
    assert(
      combine = "or",
      check_numeric(x, min.len = 1L, any.missing = FALSE),
      check_complex(x, min.len = 1L, any.missing = FALSE)
    )
    qassert(Re(x), "N+[0,)")
    qassert(cscale, "N1(0,)")
    x <- x * cscale
    x / (x + object@lambda)
  }
)
