#' @include allClass-S4.R
NULL

#' @describeIn CompleteBernsteinFunction-class
#'   returns the *Stieltjes density* with `lower`, `upper`, and `type`
#'   attributes if continuous and returns a `data.frame` with named columns
#'   `x` (atoms) and `y` (weights) as well as a type attribute if discrete.
#'   The `type` attribute is either `"continuous"` or `"discrete"`.
#'
#' @inheritParams levyDensity
#'
#' @export
setGeneric("stieltjesDensity",
  function(object) {
    standardGeneric("stieltjesDensity")
  })

#' @describeIn AlphaStableBernsteinFunction-class
#'   see [CompleteBernsteinFunction-class].
#' @aliases stieltjesDensity,AlphaStableBernsteinFunction-method
#'
#' @inheritParams levyDensity
#'
#' @section Stieltjes Density:
#' \deqn{
#'   \sigma(du)
#'     = \frac{\sin(\alpha \pi)}{\pi} u^{\alpha - 1}, \quad u > 0 .
#' }
#'
#' @export
setMethod("stieltjesDensity", "AlphaStableBernsteinFunction",
  function(object) {
    structure(
      function(x) {
        sin(object@alpha * pi) / pi * x ^ (object@alpha - 1)
      },
      lower = 0, upper = Inf, type = "continuous"
    )
  })

#' @describeIn ExponentialBernsteinFunction-class
#'   see [CompleteBernsteinFunction-class]
#' @aliases stieltjesDensity,ExponentialBernsteinFunction-method
#'
#' @inheritParams levyDensity
#'
#' @section Stieltjes density:
#' \deqn{
#'   \sigma(du)
#'     = \delta_{\lambda}(du), \quad u > 0 .
#' }
#'
#' @export
setMethod("stieltjesDensity", "ExponentialBernsteinFunction",
  function(object) {
    structure(
      data.frame(x = object@lambda, y = 1),
      type = "discrete"
    )
  })

#' @describeIn GammaBernsteinFunction-class
#'   see [CompleteBernsteinFunction-class]
#' @aliases stieltjesDensity,GammaBernsteinFunction-method
#'
#' @inheritParams levyDensity
#'
#' @section Stieltjes density:
#' \deqn{
#'   \sigma(du)
#'     = 1 / x, x > a .
#' }
#'
setMethod("stieltjesDensity", "GammaBernsteinFunction",
  function(object) {
    structure(
      function(x) {
        1 / x
      },
      lower = object@a, upper = Inf, type = "continuous"
    )
  })
