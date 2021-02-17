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

#' @describeIn InverseGaussianBernsteinFunction-class
#'   see [CompleteBernsteinFunction-class]
#' @aliases stieltjesDensity,InverseGaussianBernsteinFunction-method
#'
#' @inheritParams levyDensity
#'
#' @section Stieltjes density:
#' \deqn{
#'   \sigma(du)
#'     = \frac{\sin(\pi / 2)}{\pi} \cdot \frac{\sqrt{2 x - \eta^2}}{x} , \quad u > \eta^2 / 2 .
#' }
#'
setMethod("stieltjesDensity", "InverseGaussianBernsteinFunction",
  function(object) {
    structure(
      function(x) {
        sin(pi / 2) / pi * sqrt(2 * x - object@eta^2) / x
      },
      lower = object@eta^2 / 2, upper = Inf, type = "continuous"
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
#'     = 1 / u du, u > a .
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
