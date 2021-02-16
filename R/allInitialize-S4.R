#' @include allClass-S4.R
NULL

setMethod("initialize", "LinearBernsteinFunction",
  function(.Object, scale = 1) {
    .Object@scale <- scale
    validObject(.Object)

    invisible(.Object)
  })

setMethod("initialize", "ConstantBernsteinFunction",
  function(.Object, constant = 1) {
    .Object@constant <- constant
    validObject(.Object)

    invisible(.Object)
  })

setMethod("initialize", "ScaledBernsteinFunction",
  function(.Object, scale = 1, original = LinearBernsteinFunction()) {
    .Object@scale <- scale
    .Object@original <- original
    validObject(.Object)

    invisible(.Object)
  })

setMethod("initialize", "SumOfBernsteinFunctions",
  function(.Object, first = ConstantBernsteinFunction(0.5),
      second = LinearBernsteinFunction(0.5)) {
    .Object@first <- first
    .Object@second <- second
    validObject(.Object)

    invisible(.Object)
  })

setMethod("initialize", "PoissonBernsteinFunction",
  function(.Object, eta = 1, lambda = 1) {
    .Object@eta <- eta
    .Object@lambda <- lambda
    validObject(.Object)

    invisible(.Object)
  })

setMethod("initialize", "AlphaStableBernsteinFunction",
  function(.Object, alpha = log2(2 - 0.5)) {
    .Object@alpha <- alpha
    validObject(.Object)

    invisible(.Object)
  })

setMethod("initialize", "InverseGaussianBernsteinFunction",
  function(.Object, eta = 0.5) {
    .Object@eta <- eta
    validObject(.Object)

    invisible(.Object)
  })

setMethod("initialize", "ExponentialBernsteinFunction",
  function(.Object, lambda = 1) {
    .Object@lambda <- lambda
    validObject(.Object)

    invisible(.Object)
  })

setMethod("initialize", "GammaBernsteinFunction",
  function(.Object, a = 1) {
    .Object@a <- a
    validObject(.Object)

    invisible(.Object)
  })

setMethod("initialize", "ParetoBernsteinFunction",
  function(.Object, alpha = log2(2 - 0.5), x0 = 1e-4) {
    .Object@alpha <- alpha
    .Object@x0 <- x0
    validObject(.Object)

    invisible(.Object)
  })
