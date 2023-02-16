#' @include error.R
#' @include s4-BernsteinFunction.R
#' @include s4-LevyBernsteinFunction.R
#' @include s4-CompleteBernsteinFunction.R
NULL

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
#' @references
#'   \insertAllCited{}
#'
#' @seealso [BernsteinFunction-class], [LevyBernsteinFunction-class],
#'   [CompleteBernsteinFunction-class],
#'   [valueOf()]
#'
#' @export AlphaStableBernsteinFunction
AlphaStableBernsteinFunction <- setClass("AlphaStableBernsteinFunction", # nolint
    contains = "CompleteBernsteinFunction",
    slots = c(alpha = "numeric")
)

#' @describeIn AlphaStableBernsteinFunction-class Constructor
#' @aliases initialize,AlphaStableBernsteinFunction-method
#' @aliases initialize,AlphaStableBernsteinFunction,ANY-method
#'
#' @inheritParams methods::initialize
#' @param alpha Positive number between zero and one (bounds excl.).
#'
#' @examples
#' AlphaStableBernsteinFunction()
#' AlphaStableBernsteinFunction(alpha = 0.5)
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

#' @describeIn AlphaStableBernsteinFunction-class Display the object.
#' @aliases show,AlphaStableBernsteinFunction-method
#'
#' @export
setMethod(
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
)

#' @describeIn AlphaStableBernsteinFunction-class
#'   see [LevyBernsteinFunction-class]
#' @aliases levyDensity,AlphaStableBernsteinFunction-method
#'
#' @inheritParams levyDensity
#'
#' @section Lévy density:
#' \deqn{
#'   \nu(du)
#'     = \frac{\alpha}{\Gamma(1-\alpha)} u^{-1 - \alpha} , \quad u > 0 .
#' }
#'
#' @export
setMethod(
    "levyDensity", "AlphaStableBernsteinFunction",
    function(object) {
        structure(
            function(x) {
                object@alpha / gamma(1 - object@alpha) * x^(-1 - object@alpha)
            },
            lower = 0, upper = Inf, type = "continuous"
        )
    }
)

#' @describeIn AlphaStableBernsteinFunction-class
#'   see [CompleteBernsteinFunction-class].
#' @aliases stieltjesDensity,AlphaStableBernsteinFunction-method
#'
#' @inheritParams levyDensity
#'
#' @section Stieltjes Density:
#' \deqn{
#'   \sigma(du)
#'     = \frac{\sin(\alpha \pi)}{\pi} u^{\alpha - 1}, \quad u > 0 .
#' }
#'
#' @export
setMethod(
    "stieltjesDensity", "AlphaStableBernsteinFunction",
    function(object) {
        structure(
            function(x) {
                sin(object@alpha * pi) / pi * x^(object@alpha - 1)
            },
            lower = 0, upper = Inf, type = "continuous"
        )
    }
)

#' @keywords internal
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
