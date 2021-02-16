#' @include allClass-S4.R
NULL

setGeneric("stieltjesDensity",
  function(object) {
    standardGeneric("stieltjesDensity")
  })

setMethod("stieltjesDensity", "AlphaStableBernsteinFunction",
  function(object) {
    structure(
      function(x) {
        sin(object@alpha * pi) / pi * x^(object@alpha - 1)
      },
      lower = 0, upper = Inf, type = "continuous"
    )
  })

setMethod("stieltjesDensity", "ExponentialBernsteinFunction",
  function(object) {
    structure(
      data.frame(x = object@lambda, y = 1),
      type = "discrete"
    )
  })

setMethod("stieltjesDensity", "GammaBernsteinFunction",
  function(object) {
    structure(
      function(x) {
        1 / x
      },
      lower = object@a, upper = Inf, type = "continuous"
    )
  })
