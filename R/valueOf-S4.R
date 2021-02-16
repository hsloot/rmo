#' @include allClass-S4.R levyDensity-S4.R stieltjesDensity-S4.R
NULL

#' @describeIn BernsteinFunction-class
#'   Calculate the values for a Bernstein function and its higher-order,
#'   alternating iterated forward differences, i.e.
#'   \deqn{
#'     {(-1)}^{k-1} \Delta^k \psi(x), x > 0.
#'   }
#'
#' @inheritParams levyDensity
#' @param x Non-negativ numeric vector at which the iterated difference of
#'   the Bernstein function is evaluated.
#' @param difference_order The order of the alternating iterated forward
#'   differences taken on the Bernstein function (\eqn{k} in
#'   the representation).
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
  function(object, x, difference_order = 0L, ...) {
    standardGeneric("valueOf")
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
  function(object, x, difference_order = 0L, ...) {
    assert(combine = "or",
      check_numeric(x, lower = 0, min.len = 1L, any.missing = FALSE),
      check_complex(x, min.len = 1L, any.missing = FALSE))
    qassert(difference_order, "X1[0,)")

    if (0L == difference_order) {
      out <- object@scale * x
    } else if (1L == difference_order) {
      out <- rep(object@scale, length(x))
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
  function(object, x, difference_order = 0L, ...) {
    assert(combine = "or",
      check_numeric(x, lower = 0, min.len = 1L, any.missing = FALSE),
      check_complex(x, min.len = 1L, any.missing = FALSE))
    qassert(difference_order, "X1[0,)")

    if (0L == difference_order) {
      out <- ifelse(x == 0, 0, object@constant)
    } else {
      out <- ifelse(x == 0, object@constant, 0)
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
  function(object, x, difference_order = 0L, ...) {
    object@scale * valueOf(object@original, x, difference_order, ...)
  })


#' @describeIn SumOfBernsteinFunctions-class
#'   Calculates the iterated differences of the Bernstein function, see [valueOf()]
#' @aliases valueOf,SumOfBernsteinFunctions,ANY-method
#'
#' @inheritParams valueOf
#'
#' @export
setMethod("valueOf", "SumOfBernsteinFunctions",
  function(object, x, difference_order = 0L, ...) {
    valueOf(object@first, x, difference_order, ...) +
      valueOf(object@second, x, difference_order, ...)
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
#'   (-1)^{k-1} \Delta^{k} \psi(x)
#'     = \int_{0}^{\infty} \operatorname{e}^{-ux} (1 - \operatorname{e}^{-u})^k \nu(du) ,
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
#'   (-1)^{k-1} \Delta^{k} \psi(x)
#'     = \sum_{i} \operatorname{e}^{-u_i x} (1 - \operatorname{e}^{-u_i})^k y_i ,
#'     \quad x > 0 .
#' }
#'
#' @importFrom checkmate qassert
#' @export
setMethod("valueOf", "LevyBernsteinFunction",
  function(object, x, difference_order, ...,
      method = "levy",
      tolerance = .Machine$double.eps^0.5) {
    method <- match.arg(method)
    qassert(x, "N+[0,)")
    qassert(difference_order, "X1[0,)")
    levy_density <- levyDensity(object)
    if (0L == difference_order) {
      fct <- function(u, .x) {
        (1 - exp(-u * .x))
      }
    } else {
      fct <- function(u, .x) {
        exp(-u * .x) * (1 - exp(-u))^difference_order
      }
    }

    if ("continuous" == attr(levy_density, "type")) {
      integrand_fn <- function(u, .x) {
        fct(u, .x) * levy_density(u)
      }
      out <- sapply(x,
        function(.x) {
          integrate(integrand_fn, .x = .x,
            lower = attr(levy_density, "lower"), upper = attr(levy_density, "upper"),
            rel.tol = tolerance)$value
        })
    } else {
      out <- as.vector(levy_density$y %*% outer(levy_density$x, x, fct))
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
#'   (-1)^{k-1} \Delta^{k} \psi(x)
#'     = \int_{0}^{\infty} u \mathrm{Beta}(k+1, x + u) \sigma(du) ,
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
#'   (-1)^{k-1} \Delta^{k} \psi(x)
#'     = \sum_{i} u_i \mathrm{Beta}(k+1, x + u_i) y_i ,
#'     \quad x > 0 .
#' }
#'
#' @importFrom checkmate qassert
#' @export
setMethod("valueOf", "CompleteBernsteinFunction",
  function(object, x, difference_order, ...,
      method = c("stieltjes", "levy"),
      tolerance = .Machine$double.eps^0.5) {
    method <- match.arg(method)
    if (!"stieltjes" == method) {
      return(callNextMethod())
    }
    qassert(x, "N+[0,)")
    qassert(difference_order, "X1[0,)")
    stieltjes_density <- stieltjesDensity(object)
    if (0L == difference_order) {
      fct <- function(u, .x) {
        .x * beta(1, .x + u)
      }
    } else {
      fct <- function(u, .x) {
        u * beta(difference_order + 1L, .x + u)
      }
    }

    if ("continuous" == attr(stieltjes_density, "type")) {
      integrand_fn <- function(u, .x) {
        fct(u, .x) * stieltjes_density(u)
      }
      out <- sapply(x,
        function(.x) {
          integrate(integrand_fn, .x = .x,
            lower = attr(stieltjes_density, "lower"),
            upper = attr(stieltjes_density, "upper"),
            rel.tol = tolerance)$value
        })
    } else {
      out <- as.vector(stieltjes_density$y %*% outer(stieltjes_density$x, x, fct))
    }
  })


#' @describeIn PoissonBernsteinFunction-class
#'   Calculates the iterated differences of the Bernstein function, see [valueOf()].
#'   The default method for calculating the iterated differences uses the
#'   Lévy representation.
#' @aliases valueOf,PoissonBernsteinFunction,ANY-method
#'
#' @inheritParams valueOf
#'
#' @importFrom checkmate qassert assert check_numeric check_complex
#' @export
setMethod("valueOf", "PoissonBernsteinFunction",
  function(object, x, difference_order = 0L, ...,
      method = c("default", "levy")) {
    method <- match.arg(method)
    if (!"default" == method) {
      return(callNextMethod())
    }
    assert(combine = "or",
      check_numeric(x, lower = 0, min.len = 1L, any.missing = FALSE),
      check_complex(x, min.len = 1L, any.missing = FALSE))
    qassert(difference_order, "X1[0,)")

    if (0L == difference_order) {
      out <- object@lambda * (1 - exp(-x * object@eta))
    } else {
      out <- callNextMethod(object, x, difference_order, ..., method = "levy")
    }

    out
  })


#' @describeIn AlphaStableBernsteinFunction-class
#'   Calculates the iterated differences of the Bernstein function, see [valueOf()].
#'   The default method for calculating the iterated differences uses the
#'   Lévy representation.
#' @aliases valueOf,AlphaStableBernsteinFunction,ANY-method
#'
#' @inheritParams valueOf
#'
#' @importFrom checkmate qassert assert check_numeric check_complex
#' @export
setMethod("valueOf", "AlphaStableBernsteinFunction",
  function(object, x, difference_order = 0L, ...,
      method = c("default", "levy", "stieltjes"),
      tolerance = .Machine$double.eps^0.5) {
    method <- match.arg(method)
    if (!"default" == method) {
      return(callNextMethod())
    }
    assert(combine = "or",
      check_numeric(x, lower = 0, min.len = 1L, any.missing = FALSE),
      check_complex(x, min.len = 1L, any.missing = FALSE))
    qassert(difference_order, "X1[0,)")

    if (0L == difference_order) {
      out <- x^object@alpha
    } else {
      out <- callNextMethod(object, x, difference_order, ...,
        method = "levy", tolerance = tolerance)
    }

    out
  })


#' @describeIn InverseGaussianBernsteinFunction-class
#'   Calculates the iterated differences of the Bernstein function, see [valueOf()].
#'   The default method for calculating the iterated differences uses the
#'   Lévy representation.
#' @aliases valueOf,InverseGaussianBernsteinFunction,ANY-method
#'
#' @inheritParams valueOf
#'
#' @importFrom checkmate qassert assert check_numeric check_complex
#' @export
setMethod("valueOf", "InverseGaussianBernsteinFunction",
  function(object, x, difference_order = 0L, ...,
      method = c("default", "levy"),
      tolerance = .Machine$double.eps^0.5) {
    method <- match.arg(method)
    if (!"default" == method) {
      return(callNextMethod())
    }
    assert(combine = "or",
      check_numeric(x, lower = 0, min.len = 1L, any.missing = FALSE),
      check_complex(x, min.len = 1L, any.missing = FALSE))
    qassert(difference_order, "X1[0,)")

    if (0L == difference_order) {
      out <- sqrt(2*x + object@eta^2) - object@eta
    } else {
      out <- callNextMethod(object, x, difference_order, ...,
        method = "levy", tolerance = tolerance)
    }

    out
  })


#' @describeIn ExponentialBernsteinFunction-class
#'   Calculates the iterated differences of the Bernstein function, see [valueOf()].
#'   The default method for calculating the iterated differences uses the
#'   Stieltjes representation.
#' @aliases valueOf,ExponentialBernsteinFunction,ANY-method
#'
#' @inheritParams valueOf
#'
#' @importFrom checkmate qassert
#' @export
setMethod("valueOf", "ExponentialBernsteinFunction",
  function(object, x, difference_order = 0L, ...,
      method = c("default", "stieltjes", "levy"),
      tolerance = .Machine$double.eps^0.5) {
    method <- match.arg(method)
    if (!"default" == method) {
      return(callNextMethod())
    }
    qassert(x, "N+[0,)")
    qassert(difference_order, "X1[0,)")

    if (0L == difference_order) {
      out <- x / (x + object@lambda)
    } else {
      out <- callNextMethod(object, x, difference_order, ...,
        method = "stieltjes", tolerance = tolerance)
    }

    out
  })


#' @describeIn GammaBernsteinFunction-class
#'   Calculates the iterated differences of the Bernstein function, see [valueOf()].
#'   The default method for calculating the iterated differences uses the
#'   Lévy representation.
#' @aliases valueOf,GammaBernsteinFunction,ANY-method
#'
#' @seealso [GammaBernsteinFunction-class]
#'
#' @importFrom checkmate qassert
#' @importFrom stats integrate
#' @export
setMethod("valueOf", "GammaBernsteinFunction",
  function(object, x, difference_order = 0L, ...,
      method = c("default", "stieltjes", "levy"),
      tolerance = .Machine$double.eps^0.5) {
    method <- match.arg(method)
    if (!"default" == method) {
      return(callNextMethod())
    }
    qassert(x, "N+[0,)")
    qassert(difference_order, "X1[0,)")

    if (0L == difference_order) {
      out <- log(1 + x/object@a)
    } else {
      out <- callNextMethod(object, x, difference_order, ...,
        method = "levy", tolerance = tolerance)
    }

    out
  })


#' @describeIn ParetoBernsteinFunction-class
#'   Calculates the iterated differences of the Bernstein function, see [valueOf()].
#'   The default method for calculating the iterated differences uses the
#'   Lévy representation.
#' @aliases valueOf,ParetoBernsteinFunction,ANY-method
#'
#' @inheritParams valueOf
#'
#' @importFrom checkmate qassert
#' @importFrom stats pgamma
#' @export
setMethod("valueOf", "ParetoBernsteinFunction",
  function(object, x, difference_order = 0L, ...,
      method = c("default", "stieltjes", "levy"),
      tolerance = .Machine$double.eps^0.5) {
    method <- match.arg(method)
    if (!"default" == method) {
      return(callNextMethod())
    }
    qassert(x, "N+[0,)")
    qassert(difference_order, "X1[0,)")

    if (0L == difference_order) {
      out <- 1 - exp(-object@x0*x) + (x*object@x0) ^ (object@alpha) *
        pgamma(object@x0*x, 1-object@alpha, lower.tail=FALSE) *
        gamma(1-object@alpha)
    } else {
      out <- callNextMethod(object, x, difference_order, ...,
        method = "levy", tolerance = tolerance)
    }

    out
  })
