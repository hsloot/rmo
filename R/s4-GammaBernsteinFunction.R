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
#' The Gamma Bernstein function has the Lévy density \eqn{\nu}:
#' \deqn{
#'   \nu(du)
#'     = \frac{\operatorname{e}^{-a u}}{u}, \quad u > 0 ,
#' }
#'
#' and it has the Stieltjes density \eqn{\sigma}:
#' \deqn{
#'   \sigma(du)
#'     = u^{-1} du, u > a .
#' }
#'
#' @references
#'   \insertAllCited{}
#'
#' @seealso [getLevyDensity()], [stieltjesDensity()], [valueOf()],
#'   [calcShockArrivalIntensities()], [calcExShockArrivalIntensities()],
#'   [calcExShockSizeArrivalIntensities()], [calcMDCMGeneratorMatrix()],
#'   [rextmo()], [rpextmo()]
#'
#' @docType class
#' @name GammaBernsteinFunction-class
#' @rdname GammaBernsteinFunction-class
#' @aliases GammaBernsteinFunction
#' @include s4-BernsteinFunction.R s4-CompleteBernsteinFunction.R
#' @family Bernstein function classes
#' @family Levy Bernstein function classes
#' @family Complete Bernstein function classes
#' @family Logarithmic Bernstein function classes
#' @export GammaBernsteinFunction
#' @examples
#' # Create an object of class GammaBernsteinFunction
#' GammaBernsteinFunction()
#' GammaBernsteinFunction(a = 2)
#'
#' # Create a Lévy density
#' bf <- GammaBernsteinFunction(a = 0.7)
#' levy_density <- getLevyDensity(bf)
#' integrate(
#'   function(x) pmin(1, x) * levy_density(x),
#'   lower = attr(levy_density, "lower"),
#'   upper = attr(levy_density, "upper")
#' )
#'
#' # Create a Stieltjes density
#' bf <- GammaBernsteinFunction(a = 0.5)
#' stieltjes_density <- stieltjesDensity(bf)
#' integrate(
#'   function(x) 1/(1 + x) * stieltjes_density(x),
#'   lower = attr(stieltjes_density, "lower"),
#'   upper = attr(stieltjes_density, "upper")
#' )
#'
#' # Evaluate the Bernstein function
#' bf <- GammaBernsteinFunction(a = 0.3)
#' valueOf(bf, 1:5)
#'
#' # Calculate shock-arrival intensities
#' bf <- GammaBernsteinFunction(a = 0.8)
#' calcShockArrivalIntensities(bf, 3)
#' calcShockArrivalIntensities(bf, 3, method = "stieltjes")
#' calcShockArrivalIntensities(bf, 3, tolerance = 1e-4)
#'
#' # Calculate exchangeable shock-arrival intensities
#' bf <- GammaBernsteinFunction(a = 0.4)
#' calcExShockArrivalIntensities(bf, 3)
#' calcExShockArrivalIntensities(bf, 3, method = "stieltjes")
#' calcExShockArrivalIntensities(bf, 3, tolerance = 1e-4)
#'
#' # Calculate exchangeable shock-size arrival intensities
#' bf <- GammaBernsteinFunction(a = 0.2)
#' calcExShockSizeArrivalIntensities(bf, 3)
#' calcExShockSizeArrivalIntensities(bf, 3, method = "stieltjes")
#' calcExShockSizeArrivalIntensities(bf, 3, tolerance = 1e-4)
#'
#' # Calculate the Markov generator
#' bf <- GammaBernsteinFunction(a = 0.6)
#' calcMDCMGeneratorMatrix(bf, 3)
#' calcMDCMGeneratorMatrix(bf, 3, method = "stieltjes")
#' calcMDCMGeneratorMatrix(bf, 3, tolerance = 1e-4)
GammaBernsteinFunction <- setClass("GammaBernsteinFunction", # nolint
  contains = "CompleteBernsteinFunction",
  slots = c(a = "numeric")
)

#' @rdname hidden_aliases
#'
#' @inheritParams methods::initialize
#' @param a Positive number.
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

#' @include error.R
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

#' @rdname hidden_aliases
#'
#' @inheritParams methods::show
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

#' @rdname hidden_aliases
#'
#' @inheritParams getLevyDensity
#'
#' @include s4-getLevyDensity.R
#' @export
setMethod(
  "getLevyDensity", "GammaBernsteinFunction",
  function(object) {
    structure(
      function(x) {
        exp(-object@a * x) / x
      },
      lower = 0, upper = Inf, type = "continuous"
    )
  }
)

#' @rdname hidden_aliases
#'
#' @inheritParams stieltjesDensity
#'
#' @include s4-stieltjesDensity.R
#' @export
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

#' @rdname hidden_aliases
#'
#' @inheritParams valueOf0
#'
#' @include s4-valueOf0.R
#' @importFrom checkmate assert qassert check_numeric check_complex
#' @export
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
