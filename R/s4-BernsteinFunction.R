#' @importFrom methods new setClass setValidity setGeneric setMethod validObject
#'    callNextMethod show classLabel
NULL

#' Virtual superclass for Bernstein functions
#'
#' A virtual superclass for all implementations of the various classes of *Bernstein functions*.
#'
#' @details
#'
#' A *Bernstein function* is a non-negative, non-decreasing, infinitely often differentiable
#' function with whose recursive finite forward differences have alternating signs:
#' \deqn{
#'     {(-1)}^{i-1} \Delta^{i}{ \psi{(x)} }
#'         \geq 0 ,
#'             \quad \forall  i \in \mathbb{N}, x \geq 0 .
#' }
#' In particular, the following sequence defines an *extendible Marshall–Olkin distribution*:
#' \deqn{
#'     {(-1)}^{i-1} \Delta^{i}{ \psi{(d-i)} } ,
#'         \quad 1 \leq i \leq d .
#' }
#'
#' @references
#'   \insertRef{Schilling2012a}{rmo}
#'
#' @seealso [LevyBernsteinFunction-class], [ScaledBernsteinFunction-class],
#' [SumOfBernsteinFunctions-class], [CompositeScaledBernsteinFunction-class],
#' [CompleteBernsteinFunction-class], [ConstantBernsteinFunction-class],
#' [LinearBernsteinFunction-class], [PoissonBernsteinFunction-class],
#' [AlphaStableBernsteinFunction-class], [ExponentialBernsteinFunction-class],
#' [InverseGaussianBernsteinFunction-class], [GammaBernsteinFunction-class],
#' [ParetoBernsteinFunction-class]
#'
#' @export
setClass("BernsteinFunction", # nolint
    contains = "VIRTUAL")

#' @describeIn BernsteinFunction-class
#'   Calculate the values for a Bernstein function and its higher-order, alternating iterated
#'   forward differences, possibly scaled by a binomial coefficient, i.e.
#'   \deqn{
#'       {(-1)}^{j-1} \Delta^{j}{ \psi(c x) } ,
#'           \quad x > 0 .
#'   }
#'
#' @inheritParams levyDensity
#' @param x a non-negativ numeric vector at which the iterated difference of
#'   the Bernstein function is evaluated.
#' @param difference_order a non-negative integer with the order of the alternating iterated
#'   forward differences taken on the Bernstein function.
#' @param cscale a positive numeric scalar with the composite scaling factor.
#' @param n,k non-negative numbers for the binomial factor.
#' @param ... pass-through parameter.
#'
#' @export
setGeneric("valueOf",
    function(object, x, difference_order = 0L, n = 1L, k = 0L, cscale = 1, ...) {
        standardGeneric("valueOf")
    })

#' @keywords internal
setGeneric("valueOf0",
    function(object, x, ...) {
        standardGeneric("valueOf0")
    })

#' @keywords internal
setGeneric("defaultMethod",
    function(object) {
        standardGeneric("defaultMethod")
    })

#' @describeIn BernsteinFunction-class
#'   Calculates (unscaled) *exchangeable shock-arrival intensities*, see [rmo()] and [rexmo()].
#'
#' @inheritParams levyDensity
#' @param d a positive integer, larger than two, for the *dimension*.
#' @param ... pass-through parameter
#'
#' @export
setGeneric("uexIntensities",
    function(object, d, ...) {
        standardGeneric("uexIntensities")
    })

#' @describeIn BernsteinFunction-class
#'   Calculates the *shock-arrival intensities*, the `intensities` parameter for
#'   [rmo()].
#'
#' @inheritParams uexIntensities
#'
#' @export
setGeneric("intensities",
    function(object, d, ...) {
        standardGeneric("intensities")
    })

#' @describeIn BernsteinFunction-class
#'   Calculates *exchangeable shock-size-arrival intensities*, the `ex_intensities` parameter for
#'   [rexmo()].
#'
#' @inheritParams uexIntensities
#'
#' @export
setGeneric("exIntensities",
    function(object, d, ...) {
        standardGeneric("exIntensities")
    })

#' @describeIn BernsteinFunction-class
#'   Calculates the *infinitesimal Markov generator matrix* of the corresponding (Markovian)
#'   default-counting process, used internally by [rexmo()].
#'
#' @inheritParams uexIntensities
#'
#' @export
setGeneric("exQMatrix",
    function(object, d, ...) {
        standardGeneric("exQMatrix")
    })

setMethod("valueOf0", "BernsteinFunction",
    function(object, x, ...) {
        valueOf(object, x, difference_order = 0L, n = 1L, k = 0L, cscale = 1, ...)
    })

#' @rdname BernsteinFunction-class
#'
#' @examples
#' exIntensities(AlphaStableBernsteinFunction(4e-1), 3L)
#'
#' @export
setMethod("uexIntensities", "BernsteinFunction",
    function(object, d, ...) {
        sapply(1:d, function(i) valueOf(object, d - i, i, ...))
    })

#' @rdname BernsteinFunction-class
#'
#' @examples
#' exIntensities(AlphaStableBernsteinFunction(4e-1), 3L)
#'
#' @export
setMethod("exIntensities", "BernsteinFunction",
    function(object, d, ...) {
        if (d == 2) {
            out <- d * (valueOf0(object, d) - valueOf0(object, d - 1))
        } else {
            out <- c(
                d * (valueOf0(object, d) - valueOf0(object, d - 1)),
                sapply(2:(d - 1), function(i) valueOf(object, d - i, i, n = d, k = i, ...)))
        }

        c(out, pmax(valueOf0(object, d) - sum(out), 0))
    })

#' @rdname BernsteinFunction-class
#'
#' @examples
#' intensities(AlphaStableBernsteinFunction(4e-1), 3L)
#'
#' @export
setMethod("intensities", "BernsteinFunction",
    function(object, d, ...) {
        uexi2i(uexIntensities(object, d, ...))
    })

#' @rdname BernsteinFunction-class
#'
#' @examples
#' exQMatrix(AlphaStableBernsteinFunction(4e-1), 3L)
#'
#' @export
setMethod("exQMatrix", "BernsteinFunction",
    function(object, d, ...) {
        exi2exqm(exIntensities(object, d, ...))
    })
