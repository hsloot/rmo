#' @include allClass-S4.R
NULL

#' @describeIn LevyBernsteinFunction-class
#'   returns the *Lévy density* with `lower`, `upper`, and `type`
#'   attributes if continuous and returns a `data.frame` with named columns
#'   `x` (atoms) and `y` (weights) as well as a type attribute if discrete.
#'   The `type` attribute is either `"continuous"` or `"discrete"`.
#'
#' @param object An object deriving from [LevyBernsteinFunction-class]
#'   (for `levyDensity`) or [CompleteBernsteinFunction-class]
#'   (for `stieltjesDensity`).
#'
#' @export
setGeneric("levyDensity",
    function(object) {
        standardGeneric("levyDensity")
    })

#' @describeIn PoissonBernsteinFunction-class
#'   see [LevyBernsteinFunction-class]
#' @aliases levyDensity,PoissonBernsteinFunction-method
#'
#' @inheritParams levyDensity
#'
#' @section Lévy density:
#' \deqn{
#'   \nu(du)
#'     = \lambda \delta_{\eta}(du), \quad u > 0 .
#' }
#'
#' @export
setMethod("levyDensity", "PoissonBernsteinFunction",
    function(object) {
        structure(
            data.frame(x = object@eta, y = 1),
            type = "discrete"
        )
    })

#' @describeIn AlphaStableBernsteinFunction-class
#'   see [LevyBernsteinFunction-class]
#' @aliases levyDensity,AlphaStableBernsteinFunction-method
#'
#' @inheritParams levyDensity
#'
#' @section Lévy density:
#' \deqn{
#'   \nu(du)
#'     = \frac{\alpha}{\Gamma(1-\alpha)} u^{-1 - \alpha} , \quad u > 0 .
#' }
#'
#' @export
setMethod("levyDensity", "AlphaStableBernsteinFunction",
    function(object) {
        structure(
            function(x) {
                object@alpha / gamma(1 - object@alpha) * x ^ (-1 - object@alpha)
            },
            lower = 0, upper = Inf, type = "continuous"
        )
    })

#' @describeIn InverseGaussianBernsteinFunction-class
#'   see [LevyBernsteinFunction-class]
#' @aliases levyDensity,InverseGaussianBernsteinFunction-method
#'
#' @inheritParams levyDensity
#'
#' @section Lévy density:
#' \deqn{
#'   \nu(du)
#'     = \frac{1}{\sqrt{2 \pi u^3}} \operatorname{e}^{-\frac{1}{2} \eta^2 u} ,
#'     \quad u > 0 .
#' }
#'
#' @export
setMethod("levyDensity", "InverseGaussianBernsteinFunction",
    function(object) {
        structure(
            function(x) {
                1 / sqrt(2 * pi * x^3) * exp(-0.5 * object@eta^2 * x)
            },
            lower = 0, upper = Inf, type = "continuous"
        )
    })

#' @describeIn ExponentialBernsteinFunction-class
#'   see [LevyBernsteinFunction-class]
#' @aliases levyDensity,ExponentialBernsteinFunction-method
#'
#' @inheritParams levyDensity
#'
#' @section Lévy density:
#' \deqn{
#'   \nu(du)
#'     = \lambda \operatorname{e}^{-\lambda u}, \quad u > 0 .
#' }
#'
#' @export
setMethod("levyDensity", "ExponentialBernsteinFunction",
    function(object) {
        structure(
            function(x) {
                object@lambda * exp(-object@lambda * x)
            },
            lower = 0, upper = Inf, type = "continuous"
        )
    })

#' @describeIn GammaBernsteinFunction-class
#'   see [LevyBernsteinFunction-class]
#' @aliases levyDensity,GammaBernsteinFunction-method
#'
#' @inheritParams levyDensity
#'
#' @section Lévy density:
#' \deqn{
#'   \nu(du)
#'     = \frac{\operatorname{e}^{-a u}}{u}, \quad u > 0 .
#' }
#'
#' @export
setMethod("levyDensity", "GammaBernsteinFunction",
    function(object) {
        structure(
            function(x) {
                exp(-object@a * x) / x
            },
            lower = 0, upper = Inf, type = "continuous"
        )
    })

#' @describeIn ParetoBernsteinFunction-class
#'   see [LevyBernsteinFunction-class]
#' @aliases levyDensity,ParetoBernsteinFunction-method
#'
#' @inheritParams levyDensity
#'
#' @section Lévy density:
#' \deqn{
#'   \nu(du)
#'     = \alpha \frac{x_0^\alpha}{u^{\alpha + 1}}, \quad u > x_0 .
#' }
#'
#' @export
setMethod("levyDensity", "ParetoBernsteinFunction",
    function(object) {
        structure(
            function(x) {
                object@alpha * (object@x0 / x) ^ (object@alpha) / x
            },
            lower = object@x0, upper = Inf, type = "continuous"
        )
    })
