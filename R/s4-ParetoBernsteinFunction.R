#' @include error.R
#' @include s4-BernsteinFunction.R
#' @include s4-LevyBernsteinFunction.R
NULL

#' Class for Pareto Bernstein functions
#'
#' @slot alpha The index \eqn{\alpha}
#' @slot x0 The cutoff point \eqn{x_0}
#'
#' @description
#' For the Pareto-jump compound Poisson process with index \eqn{0 < \alpha < 1}
#' and cutoff point \eqn{x0}, the corresponding Bernstein function is
#' \deqn{
#'   \psi(x)
#'   = 1 - e^{-x x_0} + (x_0 x)^\alpha \Gamma(1-\alpha, x_0 x) ,
#'   x>0 .
#' }
#'
#' @details
#' For this Bernstein function, the higher-order alternating iterated forward
#' differences are known in closed form but cannot be evaluated numerically
#' without the danger of loss of significance. But we can use numerical
#' integration (here: [stats::integrate()]) to approximate it with the following
#' representation:
#' \deqn{
#'   {(-1)}^{k-1} \Delta^k \psi(x)
#'   = \int_{x_0}^\infty e^{-ux} (1-e^{-u})^k
#'   \alpha \frac{{x_0}^\alpha}{t^{1+\alpha}} du,
#'   x>0, k>0 .
#' }
#'
#' The Pareto Bernstein function, in combination with a linear Bernstein
#' function can be used to approximate the Bernstein function of an
#' \eqn{\alpha}-stable subordinator, see Sec. 5.3 of
#' \insertCite{Fernandez2015a}{rmo}.
#'
#' @references
#'   \insertAllCited{}
#'
#' @seealso [BernsteinFunction-class], [LevyBernsteinFunction-class],
#'   [valueOf()]
#'
#' @export ParetoBernsteinFunction
ParetoBernsteinFunction <- setClass("ParetoBernsteinFunction", # nolint
    contains = "LevyBernsteinFunction",
    slots = c(alpha = "numeric", x0 = "numeric")
)

#' @describeIn ParetoBernsteinFunction-class Constructor
#' @aliases initialize,ParetoBernsteinFunction-method
#' @aliases initialize,ParetoBernsteinFunction,ANY-method
#'
#' @inheritParams methods::initialize
#' @param alpha Positive number between zero and one (bounds excl.).
#' @param x0 Positive number.
#'
#' @examples
#' ParetoBernsteinFunction()
#' ParetoBernsteinFunction(alpha = 0.2, x0 = 1)
setMethod(
    "initialize", "ParetoBernsteinFunction",
    function(.Object, alpha, x0) { # nolint
        if (!(missing(alpha) || missing(x0))) {
            .Object@alpha <- alpha # nolint
            .Object@x0 <- x0 # nolint
            validObject(.Object)
        }

        invisible(.Object)
    }
)

#' @importFrom checkmate qtest
setValidity(
    "ParetoBernsteinFunction",
    function(object) {
        if (!qtest(object@alpha, "N1(0,1)")) {
            return(error_msg_domain("alpha", "N1(0,1)"))
        }
        if (!qtest(object@x0, "N1(0,)")) {
            return(error_msg_domain("x0", "N1(0,)"))
        }

        invisible(TRUE)
    }
)

#' @describeIn ParetoBernsteinFunction-class Display the object.
#' @aliases show,ParetoBernsteinFunction-method
#'
#' @export
setMethod( # nocov start
    "show", "ParetoBernsteinFunction",
    function(object) {
        cat(sprintf("An object of class %s\n", classLabel(class(object))))
        if (isTRUE(validObject(object, test = TRUE))) {
            cat(sprintf("- alpha: %s\n", format(object@alpha)))
            cat(sprintf("- x0: %s\n", format(object@x0)))
        } else {
            cat("\t (invalid or not initialized)\n")
        }

        invisible(NULL)
    }
) # nocov end

#' @describeIn ParetoBernsteinFunction-class
#'   see [LevyBernsteinFunction-class]
#' @aliases levyDensity,ParetoBernsteinFunction-method
#'
#' @inheritParams levyDensity
#'
#' @section Lévy density:
#' \deqn{
#'   \nu(du)
#'     = \alpha \frac{x_0^\alpha}{u^{\alpha + 1}}, \quad u > x_0 .
#' }
#'
#' @export
setMethod(
    "levyDensity", "ParetoBernsteinFunction",
    function(object) {
        structure(
            function(x) {
                object@alpha * (object@x0 / x)^(object@alpha) / x
            },
            lower = object@x0, upper = Inf, type = "continuous"
        )
    }
)

#' @importFrom stats pgamma
#' @keywords internal
setMethod(
    "valueOf0", "ParetoBernsteinFunction",
    function(object, x, cscale = 1, ...) {
        assert(
            combine = "or",
            check_numeric(x, min.len = 1L, any.missing = FALSE),
            check_complex(x, min.len = 1L, any.missing = FALSE)
        )
        qassert(Re(x), "N+[0,)")
        qassert(cscale, "N1(0,)")
        x <- x * cscale
        1 - exp(-object@x0 * x) + (object@x0 * x)^(object@alpha) *
            pgamma(object@x0 * x, 1 - object@alpha, lower.tail = FALSE) *
            gamma(1 - object@alpha)
    }
)
