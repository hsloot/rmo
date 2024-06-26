#' Class for Inverse Gaussian Bernstein function
#'
#' @slot eta The distribution parameter (drift of the
#'   underlying Gaussian process).
#'
#' @description
#' For the inverse Gaussian Lévy subordinator with \eqn{\eta > 0},
#' the corresponding Bernstein function is the function
#' \deqn{
#'   \psi(x) = \sqrt{2x + \eta^2} - \eta, x>0.
#' }
#'
#' @details
#' For the inverse Gaussian Bernstein function, the higher-order alternating
#' iterated forward differences are not known in closed-form, but
#' we can use numerical integration (here: [stats::integrate()])
#' to approximate it with the following representation:
#' \deqn{
#'  {(-1)}^{k-1} \Delta^{k} \psi(x)
#'    = \int_0^\infty e^{-ux} (1-e^{-u})^k \frac{1}{\sqrt{2\pi}
#'      u^{3/2}} e^{-\frac{1}{2}\eta^2 u} du, x>0, k>0.
#' }
#'
#' This Bernstein function can be found on p. 309 in \insertCite{Mai2017a}{rmo}.
#' Furthermore it is  a transformation of no. 2 in the list of complete
#' Bernstein functions in Chp. 16 of \insertCite{Schilling2012a}{rmo}.
#'
#' The inverse Gaussian Bernstein function has the Lévy density \eqn{\nu}:
#' \deqn{
#'   \nu(du)
#'     = \frac{1}{\sqrt{2 \pi u^3}} \operatorname{e}^{-\frac{1}{2} \eta^2 u} ,
#'     \quad u > 0 ,
#' }
#'
#' and it has the Stieltjes density \eqn{\sigma}:
#' \deqn{
#'   \sigma(du)
#'     = \frac{
#'         \sin(\pi / 2)
#'       }{
#'         \pi
#'       } \cdot \frac{
#'         \sqrt{2 x - \eta^2}
#'       }{
#'         x
#'       } ,
#'       \quad u > \eta^2 / 2 .
#' }
#'
#' @references
#'  \insertAllCited{}
#'
#' @seealso [getLevyDensity()], [getStieltjesDensity()],
#'   [calcIterativeDifference()], [calcShockArrivalIntensities()],
#'   [calcExShockArrivalIntensities()], [calcExShockSizeArrivalIntensities()],
#'   [calcMDCMGeneratorMatrix()], [rextmo()], [rpextmo()]
#'
#' @docType class
#' @name InverseGaussianBernsteinFunction-class
#' @rdname InverseGaussianBernsteinFunction-class
#' @aliases InverseGaussianBernsteinFunction
#' @include s4-BernsteinFunction.R s4-CompleteBernsteinFunction.R
#' @family Bernstein function classes
#' @family Levy Bernstein function classes
#' @family Complete Bernstein function classes
#' @family Algebraic Bernstein function classes
#' @export InverseGaussianBernsteinFunction
#' @examples
#' # Create an object of class InverseGaussianBernsteinFunction
#' InverseGaussianBernsteinFunction()
#' InverseGaussianBernsteinFunction(eta = 0.3)
#'
#' # Create a Lévy density
#' bf <- InverseGaussianBernsteinFunction(eta = 0.7)
#' levy_density <- getLevyDensity(bf)
#' integrate(
#'   function(x) pmin(1, x) * levy_density(x),
#'   lower = attr(levy_density, "lower"),
#'   upper = attr(levy_density, "upper")
#' )
#'
#' # Create a Stieltjes density
#' bf <- InverseGaussianBernsteinFunction(eta = 0.5)
#' stieltjes_density <- getStieltjesDensity(bf)
#' integrate(
#'   function(x) 1/(1 + x) * stieltjes_density(x),
#'   lower = attr(stieltjes_density, "lower"),
#'   upper = attr(stieltjes_density, "upper")
#' )
#'
#' # Evaluate the Bernstein function
#' bf <- InverseGaussianBernsteinFunction(eta = 0.3)
#' calcIterativeDifference(bf, 1:5)
#'
#' # Calculate shock-arrival intensities
#' bf <- InverseGaussianBernsteinFunction(eta = 0.8)
#' calcShockArrivalIntensities(bf, 3)
#' calcShockArrivalIntensities(bf, 3, method = "stieltjes")
#' calcShockArrivalIntensities(bf, 3, tolerance = 1e-4)
#'
#' # Calculate exchangeable shock-arrival intensities
#' bf <- InverseGaussianBernsteinFunction(eta = 0.4)
#' calcExShockArrivalIntensities(bf, 3)
#' calcExShockArrivalIntensities(bf, 3, method = "stieltjes")
#' calcExShockArrivalIntensities(bf, 3, tolerance = 1e-4)
#'
#' # Calculate exchangeable shock-size arrival intensities
#' bf <- InverseGaussianBernsteinFunction(eta = 0.2)
#' calcExShockSizeArrivalIntensities(bf, 3)
#' calcExShockSizeArrivalIntensities(bf, 3, method = "stieltjes")
#' calcExShockSizeArrivalIntensities(bf, 3, tolerance = 1e-4)
#'
#' # Calculate the Markov generator
#' bf <- InverseGaussianBernsteinFunction(eta = 0.6)
#' calcMDCMGeneratorMatrix(bf, 3)
#' calcMDCMGeneratorMatrix(bf, 3, method = "stieltjes")
#' calcMDCMGeneratorMatrix(bf, 3, tolerance = 1e-4)
InverseGaussianBernsteinFunction <- setClass("InverseGaussianBernsteinFunction", # nolint
  contains = "CompleteBernsteinFunction",
  slots = c(eta = "numeric")
)

#' @rdname hidden_aliases
#'
#' @inheritParams methods::initialize
#' @param eta Non-negative number.
setMethod(
  "initialize", "InverseGaussianBernsteinFunction",
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
  "InverseGaussianBernsteinFunction",
  function(object) {
    if (!qtest(object@eta, "N1(0,)")) {
      return(error_msg_domain("eta", "N1(0,)"))
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
  "show", "InverseGaussianBernsteinFunction",
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
#' @inheritParams getLevyDensity
#'
#' @include s4-getLevyDensity.R
#' @export
setMethod(
  "getLevyDensity", "InverseGaussianBernsteinFunction",
  function(object) {
    structure(
      function(x) {
        1 / sqrt(2 * pi * x^3) * exp(-0.5 * object@eta^2 * x)
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
  "getStieltjesDensity", "InverseGaussianBernsteinFunction",
  function(object) {
    structure(
      function(x) {
        sin(pi / 2) / pi * sqrt(2 * x - object@eta^2) / x
      },
      lower = object@eta^2 / 2, upper = Inf, type = "continuous"
    )
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
  "calcValue", "InverseGaussianBernsteinFunction",
  function(object, x, cscale = 1, ...) {
    assert(
      combine = "or",
      check_numeric(x, min.len = 1L, any.missing = FALSE),
      check_complex(x, min.len = 1L, any.missing = FALSE)
    )
    qassert(Re(x), "N+[0,)")
    qassert(cscale, "N1(0,)")
    x <- x * cscale
    sqrt(2 * x + object@eta^2) - object@eta
  }
)
