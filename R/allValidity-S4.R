#' @include allClass-S4.R
NULL

# nolint start
ERR_MSG_DOMAIN <- "Parameter %s must be of type %s"
# nolint end

#' @importFrom checkmate qtest
setValidity("LinearBernsteinFunction",
    function(object) {
        if (!qtest(object@scale, "N1[0,)")) {
            return(sprintf(ERR_MSG_DOMAIN, "scale", "N1[0,)"))
        }

        invisible(TRUE)
    })

#' @importFrom checkmate qtest
setValidity("ConstantBernsteinFunction",
    function(object) {
        if (!qtest(object@constant, "N1[0,)")) {
            return(sprintf(ERR_MSG_DOMAIN, "constant", "N1[0,)"))
        }

        invisible(TRUE)
    })

#' @importFrom checkmate qtest
setValidity("ScaledBernsteinFunction",
    function(object) {
        if (!qtest(object@scale, "N1[0,)")) {
            return(sprintf(ERR_MSG_DOMAIN, "scale", "N1[0,)"))
        }

        invisible(TRUE)
    })

#' @importFrom checkmate qtest
setValidity("CompositeScaledBernsteinFunction",
    function(object) {
        if (!qtest(object@cscale, "N1[0,)")) {
            return(sprintf(ERR_MSG_DOMAIN, "cscale", "N1[0,)"))
        }

        invisible(TRUE)
    })

#' @importFrom checkmate qtest
setValidity("PoissonBernsteinFunction",
    function(object) {
        if (!qtest(object@eta, "N1[0,)")) {
            return(sprintf(ERR_MSG_DOMAIN, "eta", "N1[0,)"))
        }

        invisible(TRUE)
    })

#' @importFrom checkmate qtest
setValidity("AlphaStableBernsteinFunction",
    function(object) {
        if (!qtest(object@alpha, "N1(0,1)")) {
            return(sprintf(ERR_MSG_DOMAIN, "alpha", "N1(0,1)"))
        }

        invisible(TRUE)
    })

#' @importFrom checkmate qtest
setValidity("InverseGaussianBernsteinFunction",
    function(object) {
        if (!qtest(object@eta, "N1(0,)")) {
            return(sprintf(ERR_MSG_DOMAIN, "eta", "N1(0,)"))
        }

        invisible(TRUE)
    })

#' @importFrom checkmate qtest
setValidity("ExponentialBernsteinFunction",
    function(object) {
        if (!qtest(object@lambda, "N1(0,)")) {
            return(sprintf(ERR_MSG_DOMAIN, "lambda", "N1(0,)"))
        }

        invisible(TRUE)
    })

#' @importFrom checkmate qtest
setValidity("GammaBernsteinFunction",
    function(object) {
        if (!qtest(object@a, "N1(0,)")) {
            return(sprintf(ERR_MSG_DOMAIN, "a", "N1(0,)"))
        }

        invisible(TRUE)
    })

#' @importFrom checkmate qtest
setValidity("ParetoBernsteinFunction",
    function(object) {
        if (!qtest(object@alpha, "N1(0,1)")) {
            return(sprintf(ERR_MSG_DOMAIN, "alpha", "N1(0,1)"))
        }
        if (!qtest(object@x0, "N1(0,)")) {
            return(sprintf(ERR_MSG_DOMAIN, "x0", "N1(0,)"))
        }

        invisible(TRUE)
    })
