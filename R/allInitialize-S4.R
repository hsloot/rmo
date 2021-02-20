#' @include allClass-S4.R
NULL

#' @describeIn LinearBernsteinFunction-class Constructor
#' @aliases initialize,LinearBernsteinFunction-method
#' @aliases initialize,LinearBernsteinFunction,ANY-method
#'
#' @inheritParams methods::initialize
#' @param scale Non-negative number.
#'
#' @examples
#' LinearBernsteinFunction()
#' LinearBernsteinFunction(scale = 2)
setMethod("initialize", "LinearBernsteinFunction",
  function(.Object, scale = 1) { # nolint
    .Object@scale <- scale
    validObject(.Object)

    invisible(.Object)
  })

#' @describeIn ConstantBernsteinFunction-class Constructor
#' @aliases initialize,ConstantBernsteinFunction-method
#' @aliases initialize,ConstantBernsteinFunction,ANY-method
#'
#' @inheritParams methods::initialize
#' @param constant Non-negative number.
#'
#' @examples
#' ConstantBernsteinFunction()
#' ConstantBernsteinFunction(constant = 0.2)
setMethod("initialize", "ConstantBernsteinFunction",
  function(.Object, constant = 1) { # nolint
    .Object@constant <- constant
    validObject(.Object)

    invisible(.Object)
  })

#' @describeIn ScaledBernsteinFunction-class Constructor
#' @aliases initialize,ScaledBernsteinFunction-method
#' @aliases initialize,ScaledBernsteinFunction,ANY-method
#'
#' @inheritParams methods::initialize
#' @param scale Positive number.
#' @param original Derives from [BernsteinFunction-class].
#'
#' @examples
#' ScaledBernsteinFunction()
#' original_bf <- AlphaStableBernsteinFunction(alpha = 0.5)
#' ScaledBernsteinFunction(scale = 2, original = original_bf)
setMethod("initialize", "ScaledBernsteinFunction",
  function(.Object, scale = 1, original = LinearBernsteinFunction()) { # nolint
    .Object@scale <- scale
    .Object@original <- original
    validObject(.Object)

    invisible(.Object)
  })

#' @describeIn SumOfBernsteinFunctions-class Constructor
#' @aliases initialize,SumOfBernsteinFunctions-method
#' @aliases initialize,SumOfBernsteinFunctions,ANY-method
#'
#' @inheritParams methods::initialize
#' @param first Derives from [BernsteinFunction-class].
#' @param second Derives from [BernsteinFunction-class].
#'
#' @examples
#' SumOfBernsteinFunctions()
#' first_bf <- LinearBernsteinFunction(scale = 0.2)
#' second_bf <- AlphaStableBernsteinFunction(alpha = 0.5)
#' SumOfBernsteinFunctions(first = first_bf, second = second_bf)
setMethod("initialize", "SumOfBernsteinFunctions",
  function(.Object, first = ConstantBernsteinFunction(0.5), # nolint
      second = LinearBernsteinFunction(0.5)) {
    .Object@first <- first
    .Object@second <- second
    validObject(.Object)

    invisible(.Object)
  })

#' @describeIn CompositeScaledBernsteinFunction-class Constructor
#' @aliases initialize,CompositeScaledBernsteinFunction-method
#' @aliases initialize,CompositeScaledBernsteinFunction,ANY-method
#'
#' @inheritParams methods::initialize
#' @param cscale Positive number.
#' @param original Derives from [BernsteinFunction-class].
#'
#' @examples
#' CompositeScaledBernsteinFunction()
#' cscale <- 0.5
#' bf_original <- AlphaStableBernsteinFunction()
#' CompositeScaledBernsteinFunction(cscale = cscale, original = bf_original)
setMethod("initialize", "CompositeScaledBernsteinFunction",
  function(.Object, cscale = 1, original = LinearBernsteinFunction()) { # nolint
    .Object@cscale <- cscale
    .Object@original <- original
    validObject(.Object)

    invisible(.Object)
  })

#' @describeIn PoissonBernsteinFunction-class Constructor
#' @aliases initialize,PoissonBernsteinFunction-method
#' @aliases initialize,PoissonBernsteinFunction,ANY-method
#'
#' @inheritParams methods::initialize
#' @param eta Positive number.
#' @param lambda Positive number
#'
#' @examples
#' PoissonBernsteinFunction()
#' PoissonBernsteinFunction(lambda = 0.2, eta = 2)
setMethod("initialize", "PoissonBernsteinFunction",
  function(.Object, eta = 1, lambda = 1) { # nolint
    .Object@eta <- eta
    .Object@lambda <- lambda
    validObject(.Object)

    invisible(.Object)
  })

#' @describeIn AlphaStableBernsteinFunction-class Constructor
#' @aliases initialize,AlphaStableBernsteinFunction-method
#' @aliases initialize,AlphaStableBernsteinFunction,ANY-method
#'
#' @inheritParams methods::initialize
#' @param alpha Positive number between zero and one (bounds excl.).
#'
#' @examples
#' AlphaStableBernsteinFunction()
#' AlphaStableBernsteinFunction(alpha = 0.5)
setMethod("initialize", "AlphaStableBernsteinFunction",
  function(.Object, alpha = log2(2 - 0.5)) { # nolint
    .Object@alpha <- alpha
    validObject(.Object)

    invisible(.Object)
  })

#' @describeIn InverseGaussianBernsteinFunction-class Constructor
#' @aliases initialize,InverseGaussianBernsteinFunction-method
#' @aliases initialize,InverseGaussianBernsteinFunction,ANY-method
#'
#' @inheritParams methods::initialize
#' @param eta Non-negative number.
#'
#' @examples
#' InverseGaussianBernsteinFunction()
#' InverseGaussianBernsteinFunction(eta = 0.3)
setMethod("initialize", "InverseGaussianBernsteinFunction",
  function(.Object, eta = 0.5) { # nolint
    .Object@eta <- eta
    validObject(.Object)

    invisible(.Object)
  })

#' @describeIn ExponentialBernsteinFunction-class Constructor
#' @aliases initialize,ExponentialBernsteinFunction-method
#' @aliases initialize,ExponentialBernsteinFunction,ANY-method
#'
#' @inheritParams methods::initialize
#' @param lambda Positive number.
#'
#' @examples
#' ExponentialBernsteinFunction()
#' ExponentialBernsteinFunction(lambda = 0.5)
setMethod("initialize", "ExponentialBernsteinFunction",
  function(.Object, lambda = 1) { # nolint
    .Object@lambda <- lambda
    validObject(.Object)

    invisible(.Object)
  })

#' @describeIn GammaBernsteinFunction-class Constructor
#' @aliases initialize,GammaBernsteinFunction-method
#' @aliases initialize,GammaBernsteinFunction,ANY-method
#'
#' @inheritParams methods::initialize
#' @param a Positive number.
#'
#' @examples
#' GammaBernsteinFunction()
#' GammaBernsteinFunction(a = 2)
setMethod("initialize", "GammaBernsteinFunction",
  function(.Object, a = 1) { # nolint
    .Object@a <- a
    validObject(.Object)

    invisible(.Object)
  })

#' @describeIn ParetoBernsteinFunction-class Constructor
#' @aliases initialize,ParetoBernsteinFunction-method
#' @aliases initialize,ParetoBernsteinFunction,ANY-method
#'
#' @inheritParams methods::initialize
#' @param alpha Positive number between zero and one (bounds excl.).
#' @param x0 Positive number.
#'
#' @examples
#' ParetoBernsteinFunction()
#' ParetoBernsteinFunction(alpha = 0.2, x0 = 1)
setMethod("initialize", "ParetoBernsteinFunction",
  function(.Object, alpha = log2(2 - 0.5), x0 = 1e-4) { # nolint
    .Object@alpha <- alpha
    .Object@x0 <- x0
    validObject(.Object)

    invisible(.Object)
  })
