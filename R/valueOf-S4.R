#' @include allClass-S4.R levyDensity-S4.R stieltjesDensity-S4.R
NULL

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

setMethod("defaultMethod", "LevyBernsteinFunction",
    function(object) {
        "levy"
    })

#' @describeIn LinearBernsteinFunction-class
#'   Calculates the iterated differences of the Bernstein function, see [valueOf()]
#' @aliases valueOf,LinearBernsteinFunction,ANY-method
#'
#' @inheritParams valueOf
#'
#' @importFrom checkmate qassert assert check_numeric check_complex
#' @export
setMethod("valueOf", "LinearBernsteinFunction",
    function(object, x, difference_order = 0L, n = 1L, k = 0L, cscale = 1, ...) {
        assert(combine = "or",
            check_numeric(x, lower = 0, min.len = 1L, any.missing = FALSE),
            check_complex(x, min.len = 1L, any.missing = FALSE))
        qassert(difference_order, "X1[0,)")
        qassert(cscale, "N1(0,)")
        qassert(n, "X1(0,)")
        qassert(k, "N1[0,)")

        if (0L == difference_order) {
            out <- multiply_binomial_coefficient((object@scale * cscale) * x, n, k)
        } else if (1L == difference_order) {
            out <- rep(multiply_binomial_coefficient((object@scale * cscale), n, k), length(x))
        } else {
            out <- rep(0, length(x))
        }

        out
    })


#' @describeIn ConstantBernsteinFunction-class
#'   Calculates the iterated differences of the Bernstein function, see [valueOf()]
#' @aliases valueOf,ConstantBernsteinFunction,ANY-method
#'
#' @inheritParams valueOf
#'
#' @importFrom checkmate qassert assert check_numeric check_complex
#' @export
setMethod("valueOf", "ConstantBernsteinFunction",
    function(object, x, difference_order = 0L, n = 1L, k = 0L, cscale = 1, ...) {
        assert(combine = "or",
            check_numeric(x, lower = 0, min.len = 1L, any.missing = FALSE),
            check_complex(x, min.len = 1L, any.missing = FALSE))
        qassert(difference_order, "X1[0,)")
        qassert(cscale, "N1(0,)")
        qassert(n, "X1(0,)")
        qassert(k, "N1[0,)")

        if (0L == difference_order) {
            out <- ifelse(x == 0, 0, multiply_binomial_coefficient(object@constant, n, k))
        } else {
            out <- ifelse(x == 0, multiply_binomial_coefficient(object@constant, n, k), 0)
        }

        out
    })


#' @describeIn ScaledBernsteinFunction-class
#'   Calculates the iterated differences of the Bernstein function, see [valueOf()]
#' @aliases valueOf,ScaledBernsteinFunction,ANY-method
#'
#' @inheritParams valueOf
#'
#' @export
setMethod("valueOf", "ScaledBernsteinFunction",
    function(object, x, difference_order = 0L, n = 1L, k = 0L, cscale = 1, ...) {
        object@scale * valueOf(object@original, x, difference_order, n, k, cscale, ...)
    })


#' @describeIn SumOfBernsteinFunctions-class
#'   Calculates the iterated differences of the Bernstein function, see [valueOf()]
#' @aliases valueOf,SumOfBernsteinFunctions,ANY-method
#'
#' @inheritParams valueOf
#'
#' @export
setMethod("valueOf", "SumOfBernsteinFunctions",
    function(object, x, difference_order = 0L, n = 1L, k = 0L, cscale = 1, ...) {
        valueOf(object@first, x, difference_order, n, k, cscale, ...) +
            valueOf(object@second, x, difference_order, n, k, cscale, ...)
    })

#' @describeIn CompositeScaledBernsteinFunction-class
#'   Calculates the iterated differences of the Bernstein function, see [valueOf()]
#' @aliases valueOf,CompositeScaledBernsteinFunction,ANY-method
#'
#' @inheritParams valueOf
#'
#' @export
setMethod("valueOf", "CompositeScaledBernsteinFunction",
    function(object, x, difference_order = 0L, n = 1, k = 0, cscale = 1, ...) {
        valueOf(object@original, x, difference_order, n, k, cscale * object@cscale, ...)
    })


#' @describeIn LevyBernsteinFunction-class
#'   Calculates the iterated differences of the Bernstein function, see [valueOf()]
#' @aliases valueOf,LevyBernsteinFunction,ANY-method
#'
#' @inheritParams valueOf
#' @param method Method to calculate the result; use `method = "levy"` for
#'   using the Lévy representation and `method = "stieltjes"` for using the
#'   Stieltjes representation.
#' @param tolerance (Relative) tolerance, passed down to [stats::integrate()]
#'
#' @details
#' For *continuous Lévy densities*, the values of the Bernstein function are
#' calculated with [stats::integrate()] by using the representation
#' \deqn{
#'   \psi(x)
#'     = \int_{0}^{\infty} (1 - \operatorname{e}^{-ux}) \nu(du), \quad x > 0 ,
#' }
#' and the values of the iterated differences are calculated by using the
#' representation
#' \deqn{
#'   (-1)^{j-1} \Delta^{j} \psi(x)
#'     = \int_{0}^{\infty} \operatorname{e}^{-ux} (1 - \operatorname{e}^{-u})^j \nu(du) ,
#'     \quad x > 0 .
#' }
#'
#' For *discrete Lévy densities* \eqn{\nu(du) = \sum_{i} y_i \delta_{u_i}(du)}, the
#' values of the Bernstein function are calculated by using the representation
#' \deqn{
#'   \psi(x)
#'     = \sum_{i} (1 - \operatorname{e}^{-u_i x}) y_i, \quad x > 0 ,
#' }
#' and the values of the iterated differences are calculated by using the
#' representation
#' \deqn{
#'   (-1)^{j-1} \Delta^{j} \psi(x)
#'     = \sum_{i} \operatorname{e}^{-u_i x} (1 - \operatorname{e}^{-u_i})^j y_i ,
#'     \quad x > 0 .
#' }
#'
#' @importFrom checkmate qassert
#' @importFrom stats integrate
#' @export
setMethod("valueOf", "LevyBernsteinFunction",
    function(object, x, difference_order, n = 1L, k = 0L, cscale = 1, ...,
             method = c("default", "levy"),
             tolerance = .Machine$double.eps^0.5) {
        method <- match.arg(method)

        if (isTRUE("default" == method)) {
            if (isTRUE(0L == difference_order)) {
                qassert(cscale, "N1(0,)")
                qassert(n, "X1(0,)")
                qassert(k, "N1[0,)")
                out <- multiply_binomial_coefficient(valueOf0(object, x * cscale), n, k)
            } else if (isTRUE(1L == difference_order)) {
                out <- multiply_binomial_coefficient(valueOf0(object, (x + 1) * cscale), n, k) -
                    multiply_binomial_coefficient(valueOf0(object, x * cscale), n, k)
            } else {
                out <- valueOf(object, x, difference_order, n, k, cscale, ...,
                    method = defaultMethod(object), tolerance = tolerance)
            }
        } else {
            qassert(x, "N+[0,)")
            qassert(difference_order, "X1[0,)")
            qassert(cscale, "N1(0,)")
            qassert(n, "X1(0,)")
            qassert(k, "N1[0,)")
            levy_density <- levyDensity(object)
            if (isTRUE(0L == difference_order)) {
                fct <- function(u, .x) {
                    (1 - exp(-u * cscale * .x))
                }
            } else {
                fct <- function(u, .x) {
                    exp(-u * cscale * .x) * (1 - exp(-u * cscale))^difference_order
                }
            }

            if (isTRUE("continuous" == attr(levy_density, "type"))) {
                integrand_fn <- function(u, .x) {
                    multiply_binomial_coefficient(fct(u, .x) * levy_density(u), n, k)
                }
                out <- sapply(x,
                    function(.x) {
                        res <- integrate(integrand_fn, .x = .x,
                            lower = attr(levy_density, "lower"),
                            upper = attr(levy_density, "upper"),
                            rel.tol = tolerance, stop.on.error = FALSE,
                            ...)
                        if (!isTRUE("OK" == res$message) && abs(.x) < 50 * .Machine$double.eps) {
                            res <- integrate(integrand_fn, .x = 50 * .Machine$double.eps,
                                lower = attr(levy_density, "lower"),
                                upper = attr(levy_density, "upper"),
                                rel.tol = tolerance, stop.on.error = FALSE,
                                ...)
                        }
                        if (!isTRUE("OK" == res$message)) {
                            stop(sprintf(
                                "Numerical integration failed with error: %s", res$message))
                        }

                        res$value
                    })
            } else {
                out <- multiply_binomial_coefficient(
                    as.vector(levy_density$y %*% outer(levy_density$x, x, fct)), n, k)
            }
        }

        out
    })

#' @describeIn CompleteBernsteinFunction-class
#'   Calculates the iterated differences of the Bernstein function, see [valueOf()]
#' @aliases valueOf,CompleteBernsteinFunction,ANY-method
#'
#' @inheritParams valueOf
#'
#' @details
#' For *continuous Stieltjes densities*, the values of the Bernstein function are
#' calculated with [stats::integrate()] by using the representation
#' \deqn{
#'   \psi(x)
#'     = \int_{0}^{\infty} x \mathrm{Beta}(1, x + u) \sigma(du), \quad x > 0 ,
#' }
#' and the values of the iterated differences are calculated by using the
#' representation
#' \deqn{
#'   (-1)^{j-1} \Delta^{j} \psi(x)
#'     = \int_{0}^{\infty} u \mathrm{Beta}(j+1, x + u) \sigma(du) ,
#'     \quad x > 0 .
#' }
#'
#' For *discrete Lévy densities* \eqn{\sigma(du) = \sum_{i} y_i \delta_{u_i}(du)}, the
#' values of the Bernstein function are calculated by using the representation
#' \deqn{
#'   \psi(x)
#'     = \sum_{i} x \mathrm{Beta}(1, x + u_i) y_i, \quad x > 0 ,
#' }
#' and the values of the iterated differences are calculated by using the
#' representation
#' \deqn{
#'   (-1)^{j-1} \Delta^{j} \psi(x)
#'     = \sum_{i} u_i \mathrm{Beta}(j+1, x + u_i) y_i ,
#'     \quad x > 0 .
#' }
#'
#' @importFrom checkmate qassert
#' @importFrom stats integrate
#' @export
setMethod("valueOf", "CompleteBernsteinFunction",
    function(object, x, difference_order, n = 1L, k = 0L, cscale = 1, ...,
             method = c("default", "stieltjes", "levy"),
             tolerance = .Machine$double.eps^0.5) {
        method <- match.arg(method)

        if (isTRUE("default" == method)) {
            if (isTRUE(0L == difference_order)) {
                qassert(cscale, "N1(0,)")
                qassert(n, "X1(0,)")
                qassert(k, "N1[0,)")
                out <- multiply_binomial_coefficient(valueOf0(object, x * cscale), n, k)
            } else if (isTRUE(1L == difference_order)) {
                out <- multiply_binomial_coefficient(valueOf0(object, (x + 1) * cscale), n, k) -
                    multiply_binomial_coefficient(valueOf0(object, x * cscale), n, k)
            } else {
                out <- valueOf(object, x, difference_order, n, k, cscale, ...,
                    method = defaultMethod(object), tolerance = tolerance)
            }
        } else if (isTRUE("stieltjes" == method)) {
            qassert(x, "N+[0,)")
            qassert(difference_order, "X1[0,)")
            qassert(cscale, "N1(0,)")
            qassert(n, "X1(0,)")
            qassert(k, "N1[0,)")
            stieltjes_density <- stieltjesDensity(object)
            if (isTRUE(0L == difference_order)) {
                fct <- function(u, .x) {
                    .x * beta(1, .x + u / cscale)
                }
            } else {
                fct <- function(u, .x) {
                    (u / cscale) * beta(difference_order + 1L, .x + u / cscale)
                }
            }
            if (isTRUE("continuous" == attr(stieltjes_density, "type"))) {
                integrand_fn <- function(u, .x) {
                    multiply_binomial_coefficient(fct(u, .x) * stieltjes_density(u), n, k)
                }
                out <- sapply(x,
                    function(.x) {
                        res <- integrate(integrand_fn, .x = .x,
                            lower = attr(stieltjes_density, "lower"),
                            upper = attr(stieltjes_density, "upper"),
                            rel.tol = tolerance, stop.on.error = FALSE,
                            ...)
                        if (!isTRUE("OK" == res$message) && abs(.x) < 50 * .Machine$double.eps) {
                            res <- integrate(integrand_fn, .x = 50 * .Machine$double.eps,
                                lower = attr(stieltjes_density, "lower"),
                                upper = attr(stieltjes_density, "upper"),
                                rel.tol = tolerance, stop.on.error = FALSE,
                                ...)
                        }
                        if (!isTRUE("OK" == res$message)) {
                            stop(sprintf(
                                "Numerical integration failed with error: %s", res$message))
                        }

                        res$value
                    })
            } else {
                out <- multiply_binomial_coefficient(
                    as.vector(stieltjes_density$y %*% outer(stieltjes_density$x, x, fct)), n, k)
            }
        } else {
            out <- callNextMethod()
        }

        out
    })

#' @importFrom checkmate qassert
#' @keywords internal
setMethod("valueOf0", "PoissonBernsteinFunction",
    function(object, x, ...) {
        assert(combine = "or",
            check_numeric(x, min.len = 1L, any.missing = FALSE),
            check_complex(x, min.len = 1L, any.missing = FALSE))
        qassert(Re(x), "N+[0,)")
        1 - exp(-x * object@eta)
    })

#' @keywords internal
setMethod("valueOf0", "AlphaStableBernsteinFunction",
    function(object, x, ...) {
        assert(combine = "or",
            check_numeric(x, min.len = 1L, any.missing = FALSE),
            check_complex(x, min.len = 1L, any.missing = FALSE))
        qassert(Re(x), "N+[0,)")
        x^object@alpha
    })

#' @keywords internal
setMethod("valueOf0", "InverseGaussianBernsteinFunction",
    function(object, x, ...) {
        assert(combine = "or",
            check_numeric(x, min.len = 1L, any.missing = FALSE),
            check_complex(x, min.len = 1L, any.missing = FALSE))
        qassert(Re(x), "N+[0,)")
        sqrt(2 * x + object@eta^2) - object@eta
    })

#' @keywords internal
setMethod("defaultMethod", "ExponentialBernsteinFunction",
    function(object) {
        "stieltjes"
    })

#' @keywords internal
setMethod("valueOf0", "ExponentialBernsteinFunction",
    function(object, x, ...) {
        assert(combine = "or",
            check_numeric(x, min.len = 1L, any.missing = FALSE),
            check_complex(x, min.len = 1L, any.missing = FALSE))
        qassert(Re(x), "N+[0,)")
        x / (x + object@lambda)
    })

#' @keywords internal
setMethod("valueOf0", "GammaBernsteinFunction",
    function(object, x, ...) {
        assert(combine = "or",
            check_numeric(x, min.len = 1L, any.missing = FALSE),
            check_complex(x, min.len = 1L, any.missing = FALSE))
        qassert(Re(x), "N+[0,)")
        log(1 + x / object@a)
    })

#' @importFrom stats pgamma
#' @keywords internal
setMethod("valueOf0", "ParetoBernsteinFunction",
    function(object, x, ...) {
        assert(combine = "or",
            check_numeric(x, min.len = 1L, any.missing = FALSE),
            check_complex(x, min.len = 1L, any.missing = FALSE))
        qassert(Re(x), "N+[0,)")
        1 - exp(-object@x0 * x) + (object@x0 * x) ^ (object@alpha) *
            pgamma(object@x0 * x, 1 - object@alpha, lower.tail = FALSE) *
            gamma(1 - object@alpha)
    })
