#' @include allClass-S4.R
NULL

#' @importFrom checkmate qassert
setValidity("LinearBernsteinFunction",
  function(object) {
    qassert(object@scale, "N1[0,)")

    invisible(TRUE)
  })

#' @importFrom checkmate qassert
setValidity("ConstantBernsteinFunction",
  function(object) {
    qassert(object@constant, "N1[0,)")

    invisible(TRUE)
  })

#' @importFrom checkmate qassert
setValidity("PoissonBernsteinFunction",
  function(object) {
    qassert(object@lambda, "N1[0,)")
    qassert(object@eta, "N1[0,)")

    invisible(TRUE)
  })

#' @importFrom checkmate qassert
setValidity("AlphaStableBernsteinFunction",
  function(object) {
    qassert(object@alpha, "N1(0,1)")

    invisible(TRUE)
  })

#' @importFrom checkmate qassert
setValidity("InverseGaussianBernsteinFunction",
  function(object) {
    qassert(object@eta, "N1(0,)")

    invisible(TRUE)
  })

#' @importFrom checkmate qassert
setValidity("ExponentialBernsteinFunction",
  function(object) {
    qassert(object@lambda, "N1(0,)")

    invisible(TRUE)
  })

#' @importFrom checkmate qassert
setValidity("GammaBernsteinFunction",
  function(object) {
    qassert(object@a, "N1(0,)")

    invisible(TRUE)
  })

#' @importFrom checkmate qassert
setValidity("ParetoBernsteinFunction",
  function(object) {
    qassert(object@alpha, "N1(0,1)")
    qassert(object@x0, "N1(0,)")

    invisible(TRUE)
  })
