#' Class for Exponential Bernstein functions
#'
#' @slot lambda The index \eqn{\lambda}.
#'
#' @description
#' For the Exponential-jump compound Poisson subordinator with
#' \eqn{\lambda > 0}, the corresponding Bernstein function is
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
#' The Exponential Bernstein function has the *Lévy density* \eqn{\nu}:
#' \deqn{
#'   \nu(du)
#'     = \lambda \operatorname{e}^{-\lambda u}, \quad u > 0 ,
#' }
#' and it has the (discrete) *Stieltjes density* \eqn{\sigma}:
#' \deqn{
#'   \sigma(du)
#'     = \delta_{\{ \lambda \}}(du), \quad u > 0 .
#' }
#'
#' @references
#'   \insertAllCited{}
#'
#' @seealso [getLevyDensity()], [getStieltjesDensity()],
#'   [calcIterativeDifference()], [calcShockArrivalIntensities()],
#'   [calcExShockArrivalIntensities()], [calcExShockSizeArrivalIntensities()],
#'   [calcMDCMGeneratorMatrix()], [rextmo()], [rpextmo()]
#'
#' @docType class
#' @name ExponentialBernsteinFunction-class
#' @rdname ExponentialBernsteinFunction-class
#' @aliases ExponentialBernsteinFunction
#' @include s4-BernsteinFunction.R s4-CompleteBernsteinFunction.R
#' @family Bernstein function classes
#' @family Levy Bernstein function classes
#' @family Complete Bernstein function classes
#' @family Algebraic Bernstein function classes
#' @export ExponentialBernsteinFunction
#' @examples
#' # Create an object of class ExponentialBernsteinFunction
#' ExponentialBernsteinFunction()
#' ExponentialBernsteinFunction(lambda = 0.5)
#'
#' # Create a Lévy density
#' bf <- ExponentialBernsteinFunction(lambda = 0.7)
#' levy_density <- getLevyDensity(bf)
#' integrate(
#'   function(x) pmin(1, x) * levy_density(x),
#'   lower = attr(levy_density, "lower"),
#'   upper = attr(levy_density, "upper")
#' )
#'
#' # Create a Stieltjes density
#' bf <- ExponentialBernsteinFunction(lambda = 0.5)
#' stieltjes_density <- getStieltjesDensity(bf)
#' sum(stieltjes_density$y * 1/(1 + stieltjes_density$x))
#'
#' # Evaluate the Bernstein function
#' bf <- ExponentialBernsteinFunction(lambda = 0.3)
#' calcIterativeDifference(bf, 1:5)
#'
#' # Calculate shock-arrival intensities
#' bf <- ExponentialBernsteinFunction(lambda = 0.8)
#' calcShockArrivalIntensities(bf, 3)
#' calcShockArrivalIntensities(bf, 3, method = "levy")
#' calcShockArrivalIntensities(bf, 3, tolerance = 1e-4)
#'
#' # Calculate exchangeable shock-arrival intensities
#' bf <- ExponentialBernsteinFunction(lambda = 0.4)
#' calcExShockArrivalIntensities(bf, 3)
#' calcExShockArrivalIntensities(bf, 3, method = "levy")
#' calcExShockArrivalIntensities(bf, 3, tolerance = 1e-4)
#'
#' # Calculate exchangeable shock-size arrival intensities
#' bf <- ExponentialBernsteinFunction(lambda = 0.2)
#' calcExShockSizeArrivalIntensities(bf, 3)
#' calcExShockSizeArrivalIntensities(bf, 3, method = "levy")
#' calcExShockSizeArrivalIntensities(bf, 3, tolerance = 1e-4)
#'
#' # Calculate the Markov generator
#' bf <- ExponentialBernsteinFunction(lambda = 0.6)
#' calcMDCMGeneratorMatrix(bf, 3)
#' calcMDCMGeneratorMatrix(bf, 3, method = "levy")
#' calcMDCMGeneratorMatrix(bf, 3, tolerance = 1e-4)
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
#' @inheritParams getLevyDensity
#'
#' @include s4-getLevyDensity.R
#' @export
setMethod(
  "getLevyDensity", "ExponentialBernsteinFunction",
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
#' @inheritParams getStieltjesDensity
#'
#' @include s4-getStieltjesDensity.R
#' @export
setMethod(
  "getStieltjesDensity", "ExponentialBernsteinFunction",
  function(object) {
    structure(
      data.frame(x = object@lambda, y = 1),
      type = "discrete"
    )
  }
)

#' @rdname hidden_aliases
#'
#' @inheritParams getDefaultMethodString
#'
#' @export
setMethod(
  "getDefaultMethodString", "ExponentialBernsteinFunction",
  function(object) {
    "stieltjes"
  }
)

#' @rdname hidden_aliases
#'
#' @inheritParams calcValue
#'
#' @include s4-calcValue.R
#' @importFrom checkmate assert qassert check_numeric check_complex
#' @export
setMethod(
  "calcValue", "ExponentialBernsteinFunction",
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
