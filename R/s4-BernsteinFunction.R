#' @importFrom methods new setClass setValidity setGeneric setMethod validObject
#'    callNextMethod show classLabel
NULL

#' Virtual superclass for Bernstein functions
#'
#' A virtual superclass for all implementations of the various classes of
#' *Bernstein functions*.
#'
#' @details
#'
#' A *Bernstein function* is a nonnegative, nondecreasing, infinitely often
#' differentiable function with whose recursive finite forward differences have
#' alternating signs:
#' \deqn{
#'     {(-1)}^{i-1} \Delta^{i}{ \psi{(x)} }
#'         \geq 0 ,
#'             \quad \forall  i \in \mathbb{N}, x \geq 0 .
#' }
#'
#' ### Extendible Marshall--Olkin distributions
#'
#' A Bernstein function an *extendible Marshall–Olkin distribution* with
#' *exchangeable shock-size arrival intensities*
#' \deqn{
#'   \eta_{i}
#'     = \binom{d}{i} {(-1)}^{i-1} \Delta^{i}{ \psi{(d-i)} } ,
#'          \quad 1 \leq i \leq d .
#' }
#'
#' The (upper triagonal) infinitesimal Markov generator of the associated
#' death-counting process is calculated recursively:
#' \deqn{
#'   q_{0, i}^\ast
#'     = \eta_{i} ,
#'       \quad i \in {\{ 1 , \ldots , d \}} ,
#' }
#' and
#' \deqn{
#'   q_{i+1, j+1}^\ast
#'     = \frac{d-j}{d-i} q_{i,j}^\ast + \frac{j+1-i}{d-i} q_{i, j+1}^\ast ,
#'      \quad 0 \leq i < j \leq d .
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
    contains = "VIRTUAL"
)

#' @describeIn BernsteinFunction-class
#'   Calculate the values for a Bernstein function and its higher-order,
#'   alternating iterated forward differences, possibly scaled by a binomial
#'   coefficient, i.e.,
#'   \deqn{
#'       {(-1)}^{j-1} \Delta^{j}{ \psi(c x) } ,
#'           \quad x > 0 .
#'   }
#'
#' @inheritParams levyDensity
#' @param x a nonnegativ numeric vector at which the iterated difference of
#'   the Bernstein function is evaluated.
#' @param difference_order a nonnegative integer with the order of the
#'   alternating iterated forward differences taken on the Bernstein function.
#' @param cscale a positive numeric scalar with the composite scaling factor.
#' @param n,k nonnegative numbers for the binomial factor.
#' @param ... pass-through parameter.
#'
#' @export
setGeneric(
    "valueOf",
    function(object, x, difference_order = 0L, n = 1L, k = 0L, cscale = 1, ...) { # nolint
        standardGeneric("valueOf")
    }
)

#' @keywords internal
setGeneric(
    "valueOf0",
    function(object, x, cscale = 1, ...) {
        standardGeneric("valueOf0")
    }
)

#' @keywords internal
setGeneric(
    "defaultMethod",
    function(object) {
        standardGeneric("defaultMethod")
    }
)

#' @describeIn BernsteinFunction-class
#'   Calculates (unscaled) *exchangeable shock-arrival intensities*,
#'   see [rmo()] and [rexmo()].
#'
#' @inheritParams levyDensity
#' @param d a positive integer, larger than two, for the *dimension*.
#' @param ... pass-through parameter
#'
#' @export
setGeneric(
    "uexIntensities",
    function(object, d, cscale = 1, ...) {
        standardGeneric("uexIntensities")
    }
)

#' @describeIn BernsteinFunction-class
#'   Calculates the *shock-arrival intensities*, the `intensities` parameter for
#'   [rmo()].
#'
#' @inheritParams uexIntensities
#'
#' @export
setGeneric(
    "intensities",
    function(object, d, cscale = 1, ...) {
        standardGeneric("intensities")
    }
)

#' @describeIn BernsteinFunction-class
#'   Calculates *exchangeable shock-size-arrival intensities*, the
#'   `ex_intensities` parameter for [rexmo()].
#'
#' @inheritParams uexIntensities
#'
#' @export
setGeneric(
    "exIntensities",
    function(object, d, cscale = 1, ...) {
        standardGeneric("exIntensities")
    }
)

#' @describeIn BernsteinFunction-class
#'   Calculates the *infinitesimal Markov generator matrix* of the corresponding
#'   (Markovian) default-counting process, used internally by [rexmo()].
#'
#' @inheritParams uexIntensities
#'
#' @export
setGeneric(
    "exQMatrix",
    function(object, d, cscale = 1, ...) {
        standardGeneric("exQMatrix")
    }
)

setMethod(
    "valueOf0", "BernsteinFunction",
    function(object, x, cscale = 1, ...) {
        valueOf(
            object, x,
            difference_order = 0L, n = 1L, k = 0L, cscale = cscale, ...
        )
    }
)

#' @rdname BernsteinFunction-class
#'
#' @examples
#' exIntensities(AlphaStableBernsteinFunction(4e-1), 3L)
#'
#' @export
setMethod(
    "uexIntensities", "BernsteinFunction",
    function(object, d, cscale = 1, ...) {
        sapply(1:d, function(i) valueOf(object, d - i, i, cscale = cscale, ...))
    }
)

#' @rdname BernsteinFunction-class
#'
#' @examples
#' exIntensities(AlphaStableBernsteinFunction(4e-1), 3L)
#'
#' @export
setMethod(
    "exIntensities", "BernsteinFunction",
    function(object, d, cscale = 1, ...) {
        if (d == 2) {
            out <- d * (
                valueOf0(object, d, cscale = cscale) -
                    valueOf0(object, d - 1, cscale = cscale)
            )
        } else {
            out <- c(
                d * (
                    valueOf0(object, d, cscale = cscale) -
                        valueOf0(object, d - 1, cscale = cscale)),
                sapply(
                    2:(d - 1),
                    function(i) {
                        valueOf(
                            object, d - i, i,
                            n = d, k = i, cscale = cscale, ...
                        )
                    }
                )
            )
        }

        c(out, pmax(valueOf0(object, d, cscale = cscale) - sum(out), 0))
    }
)

#' @rdname BernsteinFunction-class
#'
#' @examples
#' intensities(AlphaStableBernsteinFunction(4e-1), 3L)
#'
#' @export
setMethod(
    "intensities", "BernsteinFunction",
    function(object, d, cscale = 1, ...) {
        uexi2i(uexIntensities(object, d, cscale = cscale, ...))
    }
)

#' @rdname BernsteinFunction-class
#'
#' @examples
#' exQMatrix(AlphaStableBernsteinFunction(4e-1), 3L)
#'
#' @export
setMethod(
    "exQMatrix", "BernsteinFunction",
    function(object, d, cscale = 1, ...) {
        exi2exqm(exIntensities(object, d, cscale = cscale, ...))
    }
)
