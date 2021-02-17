#' @include allClass-S4.R levyDensity-S4.R stieltjesDensity-S4.R
NULL

#' @describeIn BernsteinFunction-class
#'   Calculate the values for a compound scaled Bernstein function and its higher-order,
#'   alternating iterated forward differences, possibly scaled by a binomial
#    coefficient, i.e.
#'   \deqn{
#'     {(-1)}^{j-1} \Delta^j \psi(c x), x > 0.
#'   }
#'
#' @inheritParams levyDensity
#' @param x Non-negativ numeric vector at which the iterated difference of
#'   the Bernstein function is evaluated.
#' @param difference_order The order of the alternating iterated forward
#'   differences taken on the Bernstein function (\eqn{j} in
#'   the representation).
#' @param cscale Positive number, the composit scaling factor.
#' @param n,k Non-negative numbers for the binomial factors.
#' @param ... Further parameter (passed to [stats::integrate()])
#'
#' @details
#' The method `valueOf` is implemented different, depending on the specific
#' Bernstein function to get the best possible accuracy:
#' \itemize{
#'   \item For `difference_order == 0L`, the values are calculated with closed
#'     form formulas if possible.
#'   \item For *linear Bernstein functions* and *constant Bernstein functions*
#'      the iterated differences are calculated automatically.
#'   \item For *scaled Bernstein functions* and *sums of Bernstein functions*
#'     the result is calculted recursively.
#'   \item For all other cases, the values are calculated using either the
#'     Lévy-Khintchine or the Stieltjes representation. Which one is chosen,
#'     depends on the case and can be manually overwritten with
#'     `method = "levy"` or `method = "stieltjes"`.
#' }
#'
#' @export
setGeneric("valueOf",
  function(object, x, difference_order = 0L, cscale = 1, n = 1L, k = 0L, ...) {
    standardGeneric("valueOf")
  })

#' @keywords internal
setGeneric("defaultMethod",
  function(object) {
    standardGeneric("defaultMethod")
  })

#' @keywords internal
setGeneric("valueOf0",
  function(object, x, ...) {
    standardGeneric("valueOf0")
  })

setMethod("defaultMethod", "LevyBernsteinFunction",
  function(object) {
    "levy"
  })

setMethod("valueOf0", "BernsteinFunction",
  function(object, x, ...) {
    valueOf(object, x, difference_order = 0L, cscale = 1, n = 1L, k = 0L, ...)
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
  function(object, x, difference_order = 0L, cscale = 1, n = 1L, k = 0L, ...) {
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
  function(object, x, difference_order = 0L, cscale = 1, n = 1L, k = 0L, ...) {
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
  function(object, x, difference_order = 0L, cscale = 1, n = 1L, k = 0L, ...) {
    object@scale * valueOf(object@original, x, difference_order, cscale, n, k, ...)
  })


#' @describeIn SumOfBernsteinFunctions-class
#'   Calculates the iterated differences of the Bernstein function, see [valueOf()]
#' @aliases valueOf,SumOfBernsteinFunctions,ANY-method
#'
#' @inheritParams valueOf
#'
#' @export
setMethod("valueOf", "SumOfBernsteinFunctions",
  function(object, x, difference_order = 0L, cscale = 1, n = 1L, k = 0L, ...) {
    valueOf(object@first, x, difference_order, cscale, n, k, ...) +
      valueOf(object@second, x, difference_order, cscale, n, k, ...)
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
  function(object, x, difference_order, cscale = 1, n = 1L, k = 0L, ...,
      method = c("default", "levy"),
      tolerance = .Machine$double.eps^0.5) {
    method <- match.arg(method)

    if (isTRUE("default" == method)) {
      if (isTRUE(0L == difference_order)) {
        qassert(cscale, "N1(0,)")
        qassert(n, "X1(0,)")
        qassert(k, "N1[0,)")
        out <- multiply_binomial_coefficient(valueOf0(object, x * cscale), n, k)
      } else {
        out <- valueOf(object, x, difference_order, cscale, n, k, ...,
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
            integrate(integrand_fn, .x = .x,
              lower = attr(levy_density, "lower"), upper = attr(levy_density, "upper"),
              rel.tol = tolerance)$value
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
  function(object, x, difference_order, cscale = 1, n = 1L, k = 0L, ...,
      method = c("default", "stieltjes", "levy"),
      tolerance = .Machine$double.eps^0.5) {
    method <- match.arg(method)

    if (isTRUE("default" == method)) {
      if (isTRUE(0L == difference_order)) {
        qassert(cscale, "N1(0,)")
        qassert(n, "X1(0,)")
        qassert(k, "N1[0,)")
        out <- multiply_binomial_coefficient(valueOf0(object, x * cscale), n, k)
      } else {
        out <- valueOf(object, x, difference_order, cscale, n, k, ...,
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
            integrate(integrand_fn, .x = .x,
              lower = attr(stieltjes_density, "lower"),
              upper = attr(stieltjes_density, "upper"),
              rel.tol = tolerance)$value
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
    object@lambda * (1 - exp(-x * object@eta))
  })

#' @keywords internal
setMethod("valueOf0", "AlphaStableBernsteinFunction",
  function(object, x, ...) {
    assert(combine = "or",
      check_numeric(x, min.len = 1L, any.missing = FALSE),
      check_complex(x, min.len = 1L, any.missing = FALSE))
    qassert(Re(x), "N+[0,)")
    x ^ object@alpha
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
      pgamma(object@x0 * x, 1 - object@alpha, lower.tail=FALSE) *
      gamma(1 - object@alpha)
  })
