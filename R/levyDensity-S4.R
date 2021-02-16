#' @include allClass-S4.R
NULL

setGeneric("levyDensity",
  function(object) {
    standardGeneric("levyDensity")
  })

setMethod("levyDensity", "PoissonBernsteinFunction",
  function(object) {
    structure(
      data.frame(x = object@eta, y = object@lambda),
      type = "discrete"
    )
  })

setMethod("levyDensity", "AlphaStableBernsteinFunction",
  function(object) {
    structure(
      function(x) {
        object@alpha / gamma(1 - object@alpha) * x ^ (-1 - object@alpha)
      },
      lower = 0, upper = Inf, type = "continuous"
    )
  })

setMethod("levyDensity", "InverseGaussianBernsteinFunction",
  function(object) {
    structure(
      function(x) {
        1 / sqrt(2 * pi * x ^ 3) * exp(-0.5 * object@eta ^ 2 * x)
      },
      lower = 0, upper = Inf, type = "continuous"
    )
  })

setMethod("levyDensity", "ExponentialBernsteinFunction",
  function(object) {
    structure(
      function(x) {
        object@lambda * exp(-object@lambda * x)
      },
      lower = 0, upper = Inf, type = "continuous"
    )
  })

setMethod("levyDensity", "GammaBernsteinFunction",
  function(object) {
    structure(
      function(x) {
        exp(-object@a * x) /  x
      },
      lower = 0, upper = Inf, type = "continuous"
    )
  })

setMethod("levyDensity", "ParetoBernsteinFunction",
  function(object) {
    structure(
      function(x) {
        object@alpha * (object@x0 / x) ^ (object@alpha) / x
      },
      lower = object@x0, upper = Inf, type = "continuous"
    )
  })
