#' @include allClass-S4.R levyDensity-S4.R stieltjesDensity-S4.R
NULL

#' Returns values for Bernstein functions
#'
#' @description
#' This method allows you to calculate the values for a Bernstein function and
#' its higher-order, alternating iterated forward differences, i.e.
#' \deqn{
#'   {(-1)}^{k-1} \Delta^k \psi(x), x > 0.
#' }
#'
#' @param object The Bernstein function object.
#' @param x The value at which the Bernstein function is to be evaluated.
#' @param difference_order The order of the alternating iterated forward
#'   differences taken on the Bernstein function (\eqn{k} in
#'   the representation).
#' @param ... Further parameter (passed to [stats::integrate()])
#'
#' @docType methods
#' @rdname valueOf-methods
#'
#' @seealso [BernsteinFunction-class]
#'
#' @export
setGeneric("valueOf",
  function(object, x, difference_order, ...) {
    standardGeneric("valueOf")
  })


#' @rdname valueOf-methods
#' @aliases valueOf,LinearBernsteinFunction,ANY-method
#'
#' @seealso [LinearBernsteinFunction-class]
#'
#' @importFrom checkmate qassert assert check_numeric check_complex
#'
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


#' @rdname valueOf-methods
#' @aliases valueOf,ConstantBernsteinFunction,ANY-method
#'
#' @seealso [ConstantBernsteinFunction-class]
#'
#' @importFrom checkmate qassert assert check_numeric check_complex
#'
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


#' @rdname valueOf-methods
#' @aliases valueOf,ScaledBernsteinFunction,ANY-method
#'
#' @seealso [ScaledBernsteinFunction-class]
#'
#' @export
setMethod("valueOf", "ScaledBernsteinFunction",
  function(object, x, difference_order = 0L, ...) {
    object@scale * valueOf(object@original, x, difference_order, ...)
  })


#' @rdname valueOf-methods
#' @aliases valueOf,SumOfBernsteinFunctions,ANY-method
#'
#' @seealso [SumOfBernsteinFunctions-class]
#'
#' @export
setMethod("valueOf", "SumOfBernsteinFunctions",
  function(object, x, difference_order = 0L, ...) {
    valueOf(object@first, x, difference_order, ...) +
      valueOf(object@second, x, difference_order, ...)
  })


#' @rdname valueOf-methods
#' @aliases valueOf,LevyBernsteinFunction,ANY-method
#'
#' @param method Method to calculate the result
#'
#' @seealso [LevyBernsteinFunction-class]
#'
#' @importFrom checkmate qassert
#'
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

#' @rdname valueOf-methods
#' @aliases valueOf,CompleteBernsteinFunction,ANY-method
#'
#' @param method Method to calculate the result
#'
#' @seealso [CompleteBernsteinFunction-class]
#'
#' @importFrom checkmate qassert
#'
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


#' @rdname valueOf-methods
#' @aliases valueOf,PoissonBernsteinFunction,ANY-method
#'
#' @seealso [PoissonBernsteinFunction-class]
#'
#' @importFrom checkmate qassert assert check_numeric check_complex
#'
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
      out <- callNextMethod()
    }

    out
  })


#' @rdname valueOf-methods
#' @aliases valueOf,AlphaStableBernsteinFunction,ANY-method
#'
#' @param tolerance (Relative) tolerance, passed down to [stats::integrate()]
#'
#' @seealso [AlphaStableBernsteinFunction-class]
#'
#' @importFrom checkmate qassert assert check_numeric check_complex
#' @importFrom stats integrate
#'
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


#' @rdname valueOf-methods
#' @aliases valueOf,InverseGaussianBernsteinFunction,ANY-method
#'
#' @seealso [InverseGaussianBernsteinFunction-class]
#'
#' @importFrom checkmate qassert assert check_numeric check_complex
#' @importFrom stats integrate
#'
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


#' @rdname valueOf-methods
#' @aliases valueOf,ExponentialBernsteinFunction,ANY-method
#'
#' @seealso [ExponentialBernsteinFunction-class]
#'
#' @importFrom checkmate qassert
#' @importFrom stats integrate
#'
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


#' @rdname valueOf-methods
#' @aliases valueOf,GammaBernsteinFunction,ANY-method
#'
#' @seealso [GammaBernsteinFunction-class]
#'
#' @importFrom checkmate qassert
#' @importFrom stats integrate
#'
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


#' @rdname valueOf-methods
#' @aliases valueOf,ParetoBernsteinFunction,ANY-method
#'
#' @seealso [ParetoBernsteinFunction-class]
#'
#' @importFrom checkmate qassert
#' @importFrom stats integrate pgamma
#'
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
