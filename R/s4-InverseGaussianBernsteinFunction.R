#' @include error.R
#' @include s4-BernsteinFunction.R
#' @include s4-LevyBernsteinFunction.R
#' @include s4-CompleteBernsteinFunction.R
NULL

#' Class for Inverse Gaussian Bernstein function
#'
#' @slot eta The distribution parameter (drift of the
#'   underlying Gaussian process)
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
#' @references
#'  \insertAllCited{}
#'
#' @seealso [BernsteinFunction-class], [LevyBernsteinFunction-class],
#'   [valueOf()]
#'
#' @export InverseGaussianBernsteinFunction
InverseGaussianBernsteinFunction <- setClass("InverseGaussianBernsteinFunction", # nolint
    contains = "CompleteBernsteinFunction",
    slots = c(eta = "numeric")
)

#' @describeIn InverseGaussianBernsteinFunction-class Constructor
#' @aliases initialize,InverseGaussianBernsteinFunction-method
#' @aliases initialize,InverseGaussianBernsteinFunction,ANY-method
#'
#' @inheritParams methods::initialize
#' @param eta Non-negative number.
#'
#' @examples
#' InverseGaussianBernsteinFunction()
#' InverseGaussianBernsteinFunction(eta = 0.3)
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

#' @describeIn InverseGaussianBernsteinFunction-class Display the object.
#' @aliases show,InverseGaussianBernsteinFunction-method
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

#' @describeIn InverseGaussianBernsteinFunction-class
#'   see [LevyBernsteinFunction-class]
#' @aliases levyDensity,InverseGaussianBernsteinFunction-method
#'
#' @inheritParams levyDensity
#'
#' @section Lévy density:
#' \deqn{
#'   \nu(du)
#'     = \frac{1}{\sqrt{2 \pi u^3}} \operatorname{e}^{-\frac{1}{2} \eta^2 u} ,
#'     \quad u > 0 .
#' }
#'
#' @export
setMethod(
    "levyDensity", "InverseGaussianBernsteinFunction",
    function(object) {
        structure(
            function(x) {
                1 / sqrt(2 * pi * x^3) * exp(-0.5 * object@eta^2 * x)
            },
            lower = 0, upper = Inf, type = "continuous"
        )
    }
)

#' @describeIn InverseGaussianBernsteinFunction-class
#'   see [CompleteBernsteinFunction-class]
#' @aliases stieltjesDensity,InverseGaussianBernsteinFunction-method
#'
#' @inheritParams levyDensity
#'
#' @section Stieltjes density:
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
setMethod(
    "stieltjesDensity", "InverseGaussianBernsteinFunction",
    function(object) {
        structure(
            function(x) {
                sin(pi / 2) / pi * sqrt(2 * x - object@eta^2) / x
            },
            lower = object@eta^2 / 2, upper = Inf, type = "continuous"
        )
    }
)

#' @keywords internal
setMethod(
    "valueOf0", "InverseGaussianBernsteinFunction",
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
