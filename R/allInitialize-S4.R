#' @include allClass-S4.R
NULL

#' Constructor for *linear Bernstein functions*
#'
#' @inheritParams methods::initialize
#' @param scale Non-negative number.
#'
#' @examples
#' LinearBernsteinFunction()
#' LinearBernsteinFunction(scale = 2)
#'
#' @seealso [LinearBernsteinFunction-class]
#'
#' @name LinearBernsteinFunction-constructor
#' @aliases initialize,LinearBernsteinFunction-method
#' @aliases initialize,LinearBernsteinFunction,ANY-method
#' @docType methods
setMethod("initialize", "LinearBernsteinFunction",
  function(.Object, scale = 1) {
    .Object@scale <- scale
    validObject(.Object)

    invisible(.Object)
  })

#' Constructor for `constant Bernstein functions*
#'
#' @inheritParams methods::initialize
#' @param constant Non-negative number.
#'
#' @seealso [ConstantBernsteinFunction-class]
#'
#' @examples
#' ConstantBernsteinFunction()
#' ConstantBernsteinFunction(constant = 0.2)
#'
#' @name ConstantBernsteinFunction-constructor
#' @aliases initialize,ConstantBernsteinFunction-method
#' @aliases initialize,ConstantBernsteinFunction,ANY-method
#' @docType methods
setMethod("initialize", "ConstantBernsteinFunction",
  function(.Object, constant = 1) {
    .Object@constant <- constant
    validObject(.Object)

    invisible(.Object)
  })

#' Constructor for a *scaled Bernstein function*
#'
#' @inheritParams methods::initialize
#' @param scale Positive number.
#' @param original Derives from [BernsteinFunction-class].
#'
#' @seealso [ScaledBernsteinFunction-class]
#'
#' @examples
#' ScaledBernsteinFunction()
#' original_bf <- AlphaStableBernsteinFunction(alpha = 0.5)
#' ScaledBernsteinFunction(scale = 2, original = original_bf)
#'
#' @name ScaledBernsteinFunction-constructor
#' @aliases initialize,ScaledBernsteinFunction-method
#' @aliases initialize,ScaledBernsteinFunction,ANY-method
#' @docType methods
setMethod("initialize", "ScaledBernsteinFunction",
  function(.Object, scale = 1, original = LinearBernsteinFunction()) {
    .Object@scale <- scale
    .Object@original <- original
    validObject(.Object)

    invisible(.Object)
  })

#' Constructor for a *sum of Bernstein functions*
#'
#' @inheritParams methods::initialize
#' @param first Derives from [BernsteinFunction-class].
#' @param second Derives from [BernsteinFunction-class].
#'
#' @seealso [SumOfBernsteinFunctions-class]
#'
#' @examples
#' SumOfBernsteinFunctions()
#' first_bf <- LinearBernsteinFunction(scale = 0.2)
#' second_bf <- AlphaStableBernsteinFunction(alpha = 0.5)
#' SumOfBernsteinFunctions(first = first_bf, second = second_bf)
#'
#' @name SumOfBernsteinFunctions-constructor
#' @aliases initialize,SumOfBernsteinFunctions-method
#' @aliases initialize,SumOfBernsteinFunctions,ANY-method
#' @docType methods
setMethod("initialize", "SumOfBernsteinFunctions",
  function(.Object, first = ConstantBernsteinFunction(0.5),
      second = LinearBernsteinFunction(0.5)) {
    .Object@first <- first
    .Object@second <- second
    validObject(.Object)

    invisible(.Object)
  })

#' Constructor of *Poisson Bernstein function*
#'
#' @inheritParams methods::initialize
#' @param eta Positive number.
#' @param lambda Positive number
#'
#' @seealso [PoissonBernsteinFunction-class]
#'
#' @examples
#' PoissonBernsteinFunction()
#' PoissonBernsteinFunction(lambda = 0.2, eta = 2)
#'
#' @name PoissonBernsteinFunction-constructor
#' @aliases initialize,PoissonBernsteinFunction-method
#' @aliases initialize,PoissonBernsteinFunction,ANY-method
#' @docType methods
setMethod("initialize", "PoissonBernsteinFunction",
  function(.Object, eta = 1, lambda = 1) {
    .Object@eta <- eta
    .Object@lambda <- lambda
    validObject(.Object)

    invisible(.Object)
  })

#' Constructor of *\eqn{\alpha}-stable Bernstein function*
#'
#' @inheritParams methods::initialize
#' @param alpha Positive number between zero and one (bounds excl.).
#'
#' @seealso [AlphaStableBernsteinFunction-class]
#'
#' @examples
#' AlphaStableBernsteinFunction()
#' AlphaStableBernsteinFunction(alpha = 0.5)
#'
#' @name AlphaStableBernsteinFunction-constructor
#' @aliases initialize,AlphaStableBernsteinFunction-method
#' @aliases initialize,AlphaStableBernsteinFunction,ANY-method
#' @docType methods
setMethod("initialize", "AlphaStableBernsteinFunction",
  function(.Object, alpha = log2(2 - 0.5)) {
    .Object@alpha <- alpha
    validObject(.Object)

    invisible(.Object)
  })

#' Constructor of *inverse Gaussian Bernstein function*
#'
#' @inheritParams methods::initialize
#' @param eta Non-negative number.
#'
#' @seealso [InverseGaussianBernsteinFunction-class]
#'
#' @examples
#' InverseGaussianBernsteinFunction()
#' InverseGaussianBernsteinFunction(eta = 0.3)
#'
#' @name InverseGaussianBernsteinFunction-constructor
#' @aliases initialize,InverseGaussianBernsteinFunction-method
#' @aliases initialize,InverseGaussianBernsteinFunction,ANY-method
#' @docType methods
setMethod("initialize", "InverseGaussianBernsteinFunction",
  function(.Object, eta = 0.5) {
    .Object@eta <- eta
    validObject(.Object)

    invisible(.Object)
  })

#' Constructor of the *Exponential Bernstein function*
#'
#' @inheritParams methods::initialize
#' @param lambda Positive number.
#'
#' @seealso [ExponentialBernsteinFunction-class]
#'
#' @examples
#' ExponentialBernsteinFunction()
#' ExponentialBernsteinFunction(lambda = 0.5)
#'
#' @name ExponentialBernsteinFunction-constructor
#' @aliases initialize,ExponentialBernsteinFunction-method
#' @aliases initialize,ExponentialBernsteinFunction,ANY-method
#' @docType methods
setMethod("initialize", "ExponentialBernsteinFunction",
  function(.Object, lambda = 1) {
    .Object@lambda <- lambda
    validObject(.Object)

    invisible(.Object)
  })

#' Constructor of the *\eqn{\Gamma} Bernstein function*
#'
#' @inheritParams methods::initialize
#' @param a Positive number
#'
#' @seealso [GammaBernsteinFunction-class]
#'
#' @examples
#' GammaBernsteinFunction()
#' GammaBernsteinFunction(a = 2)
#'
#' @name GammaBernsteinFunction-constructor
#' @aliases initialize,GammaBernsteinFunction-method
#' @aliases initialize,GammaBernsteinFunction,ANY-method
#' @docType methods
setMethod("initialize", "GammaBernsteinFunction",
  function(.Object, a = 1) {
    .Object@a <- a
    validObject(.Object)

    invisible(.Object)
  })

#' Constructor of the *Pareto Bernstein function*
#'
#' @inheritParams methods::initialize
#' @param alpha Positive number between zero and one (bounds excl.).
#' @param x0 Positive number.
#'
#' @seealso [ParetoBernsteinFunction-class]
#'
#' @examples
#' ParetoBernsteinFunction()
#' ParetoBernsteinFunction(alpha = 0.2, x0 = 1)
#'
#' @name ParetoBernsteinFunction-constructor
#' @aliases initialize,ParetoBernsteinFunction-method
#' @aliases initialize,ParetoBernsteinFunction,ANY-method
#' @docType methods
setMethod("initialize", "ParetoBernsteinFunction",
  function(.Object, alpha = log2(2 - 0.5), x0 = 1e-4) {
    .Object@alpha <- alpha
    .Object@x0 <- x0
    validObject(.Object)

    invisible(.Object)
  })
