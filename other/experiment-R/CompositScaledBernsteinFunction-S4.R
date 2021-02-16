devtools::load_all()
library(checkmate)

AlgebraicBernsteinFunction2 <- setClass("AlgebraicBernsteinFunction2",
  contains = "CompleteBernsteinFunction",
  slots = c(alpha = "numeric"))

setValidity("AlgebraicBernsteinFunction2",
  function(object) {
    qassert(object@alpha, "N1(0,)")

    invisible(TRUE)
  })

setMethod("initialize", "AlgebraicBernsteinFunction2",
  function(.Object, alpha = log2(2 - 0.5)) { # nolint
    .Object@alpha <- alpha
    validObject(.Object)

    invisible(.Object)
  })

setMethod("levyDensity", "AlgebraicBernsteinFunction2",
  function(object) {
    structure(
      function(x) {
        object@alpha / gamma(1 - object@alpha) * x ^ (-1 - object@alpha) * exp(-x)
      },
      lower = 0, upper = Inf, type = "continuous"
    )
  })

setMethod("stieltjesDensity", "AlgebraicBernsteinFunction2",
  function(object) {
    structure(
      function(x) {
        sin(object@alpha * pi) / pi * (x - 1) ^ (object@alpha - 1) / x
      },
      lower = 1, upper = Inf, type = "continuous"
    )
  })

setMethod("valueOf", "AlgebraicBernsteinFunction2",
  function(object, x, difference_order = 0L, cscale = 1, n = 1, k = 0, ...,
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
    qassert(cscale, "N1(0,)")
    qassert(n, "X1(0,)")
    qassert(k, "N1[0,)")

    if (0L == difference_order) {
      out <- multiply_binomial_coefficient((x * cscale + 1) ^ object@alpha - 1, n, k)
    } else {
      out <- callNextMethod(object, x, difference_order, cscale, n, k, ...,
                            method = "levy", tolerance = tolerance)
    }

    out
  })


CompositScaledBernsteinFunction <- setClass("CompositScaledBernsteinFunction",
  contains  = "BernsteinFunction",
  slots = c(cscale = "numeric", original = "BernsteinFunction"))

setValidity("CompositScaledBernsteinFunction",
  function(object) {
    qassert(object@cscale, "N1(0,)")

    invisible(TRUE)
  })

setMethod("initialize", "CompositScaledBernsteinFunction",
  function(.Object, cscale = 1, original = AlgebraicBernsteinFunction2()) { # nolint
    .Object@cscale <- cscale
    .Object@original <- original
    validObject(.Object)

    invisible(.Object)
  })


setMethod("valueOf", "CompositScaledBernsteinFunction",
  function(object, x, difference_order = 0L, cscale = 1, n = 1, k = 0, ...,
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
    qassert(cscale, "N1(0,)")
    qassert(n, "X1(0,)")
    qassert(k, "N1[0,)")

    valueOf(object@original, x, difference_order, cscale*object@cscale, n, k, ...)
  })


eta <- 2
valueOf(InverseGaussianBernsteinFunction(eta = eta), seq(0, 2, by = 0.25), 0L, cscale = 1)

valueOf(
  ScaledBernsteinFunction(
    scale = eta,
    original = CompositScaledBernsteinFunction(
      cscale = 2 / eta^2,
      original = AlgebraicBernsteinFunction2(
        alpha = 0.5
        )
      )
    ),
  seq(0, 2, by = 0.25))
