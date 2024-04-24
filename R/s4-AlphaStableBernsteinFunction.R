#' Class for the \eqn{\alpha}-stable Bernstein function
#'
#' @slot alpha The index \eqn{\alpha}.
#'
#' @description
#' For the \eqn{\alpha}-stable Lévy subordinator with \eqn{0 < \alpha < 1},
#' the corresponding Bernstein function is the power function with exponent
#' \eqn{\alpha}, i.e.
#' \deqn{
#'   \psi(x) = x^\alpha, \quad x>0.
#' }
#'
#' @details
#' For the \eqn{\alpha}-stable Bernstein function, the higher order alternating
#' iterated forward differences are known in closed form but cannot be evaluated
#' numerically without the danger of loss of significance. But we can use
#' numerical integration (here: [stats::integrate()]) to approximate it with the
#' following representation:
#' \deqn{
#'   {(-1)}^{k-1} \Delta^k \psi(x)
#'    = \int_0^\infty e^{-ux} (1-e^{-u})^k
#'      \alpha \frac{1}{\Gamma(1-\alpha) u^{1+\alpha}} du, x>0, k>0 .
#' }
#'
#' This Bernstein function is no. 1 in the list of complete Bernstein functions
#' in Chp. 16 of \insertCite{Schilling2012a}{rmo}.
#'
#' The \eqn{\alpha}-stable Bernstein function has the Lévy density \eqn{\nu}:
#' \deqn{
#'   \nu(du)
#'     = \frac{\alpha}{\Gamma(1-\alpha)} u^{-1 - \alpha} , \quad u > 0 ,
#' }
#' and it has the Stieltjes density \eqn{\sigma}:
#' \deqn{
#'   \sigma(du)
#'     = \frac{\sin(\alpha \pi)}{\pi} u^{\alpha - 1}, \quad u > 0 .
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
#' @name AlphaStableBernsteinFunction-class
#' @rdname AlphaStableBernsteinFunction-class
#' @aliases AlphaStableBernsteinFunction
#' @include s4-BernsteinFunction.R s4-CompleteBernsteinFunction.R
#' @family Bernstein function classes
#' @family Levy Bernstein function classes
#' @family Complete Bernstein function classes
#' @family Algebraic Bernstein function classes
#' @export AlphaStableBernsteinFunction
#' @examples
#' # Create an object of class AlphaStableBernsteinFunction
#' AlphaStableBernsteinFunction()
#' AlphaStableBernsteinFunction(alpha = 0.5)
#'
#' # Create a Lévy density
#' bf <- AlphaStableBernsteinFunction(alpha = 0.7)
#' levy_density <- getLevyDensity(bf)
#' integrate(
#'   function(x) pmin(1, x) * levy_density(x),
#'   lower = attr(levy_density, "lower"),
#'   upper = attr(levy_density, "upper")
#' )
#'
#' # Create a Stieltjes density
#' bf <- AlphaStableBernsteinFunction(alpha = 0.5)
#' stieltjes_density <- getStieltjesDensity(bf)
#' integrate(
#'   function(x) 1/(1 + x) * stieltjes_density(x),
#'   lower = attr(stieltjes_density, "lower"),
#'   upper = attr(stieltjes_density, "upper")
#' )
#'
#' # Evaluate the Bernstein function
#' bf <- AlphaStableBernsteinFunction(alpha = 0.3)
#' calcIterativeDifference(bf, 1:5)
#'
#' # Calculate shock-arrival intensities
#' bf <- AlphaStableBernsteinFunction(alpha = 0.8)
#' calcShockArrivalIntensities(bf, 3)
#' calcShockArrivalIntensities(bf, 3, method = "stieltjes")
#' calcShockArrivalIntensities(bf, 3, tolerance = 1e-4)
#'
#' # Calculate exchangeable shock-arrival intensities
#' bf <- AlphaStableBernsteinFunction(alpha = 0.4)
#' calcExShockArrivalIntensities(bf, 3)
#' calcExShockArrivalIntensities(bf, 3, method = "stieltjes")
#' calcExShockArrivalIntensities(bf, 3, tolerance = 1e-4)
#'
#' # Calculate exchangeable shock-size arrival intensities
#' bf <- AlphaStableBernsteinFunction(alpha = 0.2)
#' calcExShockSizeArrivalIntensities(bf, 3)
#' calcExShockSizeArrivalIntensities(bf, 3, method = "stieltjes")
#' calcExShockSizeArrivalIntensities(bf, 3, tolerance = 1e-4)
#'
#' # Calculate the Markov generator
#' bf <- AlphaStableBernsteinFunction(alpha = 0.6)
#' calcMDCMGeneratorMatrix(bf, 3)
#' calcMDCMGeneratorMatrix(bf, 3, method = "stieltjes")
#' calcMDCMGeneratorMatrix(bf, 3, tolerance = 1e-4)
AlphaStableBernsteinFunction <- setClass("AlphaStableBernsteinFunction", # nolint
  contains = "CompleteBernsteinFunction",
  slots = c(alpha = "numeric")
)

#' @rdname hidden_aliases
#'
#' @inheritParams methods::initialize
#' @param alpha Positive number between zero and one (bounds excl.).
setMethod(
  "initialize", "AlphaStableBernsteinFunction",
  function(.Object, alpha) { # nolint
    if (!missing(alpha)) {
      .Object@alpha <- alpha # nolint
      validObject(.Object)
    }

    invisible(.Object)
  }
)

#' @include error.R
#' @importFrom checkmate qtest
setValidity(
  "AlphaStableBernsteinFunction",
  function(object) {
    if (!qtest(object@alpha, "N1(0,1)")) {
      return(error_msg_domain("alpha", "N1(0,1)"))
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
  "show", "AlphaStableBernsteinFunction",
  function(object) {
    cat(sprintf("An object of class %s\n", classLabel(class(object))))
    if (isTRUE(validObject(object, test = TRUE))) {
      cat(sprintf("- alpha: %s\n", format(object@alpha)))
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
  "getLevyDensity", "AlphaStableBernsteinFunction",
  function(object) {
    structure(
      function(x) {
        object@alpha / gamma(1 - object@alpha) * x^(-1 - object@alpha)
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
  "getStieltjesDensity", "AlphaStableBernsteinFunction",
  function(object) {
    structure(
      function(x) {
        sin(object@alpha * pi) / pi * x^(object@alpha - 1)
      },
      lower = 0, upper = Inf, type = "continuous"
    )
  }
)

#' @rdname hidden_aliases
#'
#' @inheritParams valueOf0
#'
#' @include s4-valueOf0.R
#' @importFrom checkmate assert qassert check_numeric check_complex
#' @export
setMethod(
  "valueOf0", "AlphaStableBernsteinFunction",
  function(object, x, cscale = 1, ...) {
    assert(
      combine = "or",
      check_numeric(x, min.len = 1L, any.missing = FALSE),
      check_complex(x, min.len = 1L, any.missing = FALSE)
    )
    qassert(Re(x), "N+[0,)")
    qassert(cscale, "N1(0,)")
    x <- x * cscale
    x^object@alpha
  }
)
