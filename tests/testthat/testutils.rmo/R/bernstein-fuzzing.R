#' Fuzzing parameters of Bernstein functions
#'
#' @description
#' This method allows the fuzzing, i.e. random generation, of parametrizations
#' for Bernstein functions. Positive values are generated from the unit
#' exponential distribution and interval bounded values are generated from
#' a suitable uniform distribution.
#'
#' @param bf An instance of the Bernstein function
#'
#' @docType methods
#' @rdname fuzzy_bf-methods
#'
#' @seealso [rmo::BernsteinFunction-class]
#'
#' @importFrom methods setGeneric
#' @export
setGeneric(
  "fuzzy_bf",
  def = function(bf) {
    standardGeneric("fuzzy_bf")
  }
)


#' @rdname fuzzy_bf-methods
#' @aliases fuzzy_bf,AlphaStableBernsteinFunction,ANY-method
#'
#' @seealso [rmo::AlphaStableBernsteinFunction-class]
#' @examples
#' bf <- fuzzy_bf(rmo::AlphaStableBernsteinFunction())
#' @importFrom methods setMethod validObject
#' @importFrom rmo AlphaStableBernsteinFunction
#' @importFrom stats runif
#' @export
setMethod(
  "fuzzy_bf", "AlphaStableBernsteinFunction",
  function(bf) {
    bf@alpha <- stats::runif(1)
    validObject(bf)

    bf
  }
)

#' @rdname fuzzy_bf-methods
#' @aliases fuzzy_bf,ConstantBernsteinFunction,ANY-method
#'
#' @seealso [rmo::ConstantBernsteinFunction-class]
#' @examples
#' bf <- fuzzy_bf(rmo::ConstantBernsteinFunction())
#' @importFrom methods setMethod validObject
#' @importFrom rmo ConstantBernsteinFunction
#' @importFrom stats rexp
#' @export
setMethod(
  "fuzzy_bf", "ConstantBernsteinFunction",
  function(bf) {
    bf@constant <- stats::rexp(1)
    validObject(bf)

    bf
  }
)

#' @rdname fuzzy_bf-methods
#' @aliases fuzzy_bf,ExponentialBernsteinFunction,ANY-method
#'
#' @seealso [rmo::ExponentialBernsteinFunction-class]
#' @examples
#' bf <- fuzzy_bf(rmo::ExponentialBernsteinFunction())
#' @importFrom methods setMethod validObject
#' @importFrom rmo ExponentialBernsteinFunction
#' @importFrom stats rexp
#' @export
setMethod(
  "fuzzy_bf", "ExponentialBernsteinFunction",
  function(bf) {
    bf@lambda <- stats::rexp(1)
    validObject(bf)

    bf
  }
)

#' @rdname fuzzy_bf-methods
#' @aliases fuzzy_bf,GammaBernsteinFunction,ANY-method
#'
#' @seealso [rmo::GammaBernsteinFunction-class]
#' @examples
#' bf <- fuzzy_bf(rmo::GammaBernsteinFunction())
#' @importFrom methods setMethod validObject
#' @importFrom rmo GammaBernsteinFunction
#' @importFrom stats rexp
#' @export
setMethod(
  "fuzzy_bf", "GammaBernsteinFunction",
  function(bf) {
    bf@a <- stats::rexp(1)
    validObject(bf)

    bf
  }
)

#' @rdname fuzzy_bf-methods
#' @aliases fuzzy_bf,InverseGaussianBernsteinFunction,ANY-method
#'
#' @seealso [rmo::InverseGaussianBernsteinFunction-class]
#' @examples
#' bf <- fuzzy_bf(rmo::InverseGaussianBernsteinFunction())
#' @importFrom methods setMethod validObject
#' @importFrom rmo InverseGaussianBernsteinFunction
#' @importFrom stats rexp
#' @export
setMethod(
  "fuzzy_bf", "InverseGaussianBernsteinFunction",
  function(bf) {
    bf@eta <- stats::rexp(1)
    validObject(bf)

    bf
  }
)

#' @rdname fuzzy_bf-methods
#' @aliases fuzzy_bf,LinearBernsteinFunction,ANY-method
#'
#' @seealso [rmo::LinearBernsteinFunction-class]
#' @examples
#' bf <- fuzzy_bf(rmo::LinearBernsteinFunction())
#' @importFrom methods setMethod validObject
#' @importFrom rmo LinearBernsteinFunction
#' @importFrom stats rexp
#' @export
setMethod(
  "fuzzy_bf", "LinearBernsteinFunction",
  function(bf) {
    bf@scale <- stats::rexp(1)
    validObject(bf)

    bf
  }
)

#' @rdname fuzzy_bf-methods
#' @aliases fuzzy_bf,ParetoBernsteinFunction,ANY-method
#'
#' @details
#' For [rmo::ParetoBernsteinFunction-class], the fuzzing samples `x0` from the
#' values `c(1e-4, 1e-2, 1, 2)`.
#'
#' @seealso [rmo::ParetoBernsteinFunction-class]
#' @examples
#' bf <- fuzzy_bf(rmo::ParetoBernsteinFunction())
#' @importFrom methods setMethod validObject
#' @importFrom rmo ParetoBernsteinFunction
#' @importFrom stats runif
#' @export
setMethod(
  "fuzzy_bf", "ParetoBernsteinFunction",
  function(bf) {
    bf@alpha <- stats::runif(1)
    bf@x0 <- sample(c(1e-4, 1e-2, 1, 2), 1)
    validObject(bf)

    bf
  }
)

#' @rdname fuzzy_bf-methods
#' @aliases fuzzy_bf,PoissonBernsteinFunction,ANY-method
#'
#' @seealso [rmo::PoissonBernsteinFunction-class]
#' @examples
#' bf <- fuzzy_bf(rmo::PoissonBernsteinFunction())
#' @importFrom methods setMethod validObject
#' @importFrom rmo PoissonBernsteinFunction
#' @importFrom stats rexp
#' @export
setMethod(
  "fuzzy_bf", "PoissonBernsteinFunction",
  function(bf) {
    bf@lambda <- stats::rexp(1)
    bf@eta <- stats::rexp(1)
    validObject(bf)

    bf
  }
)

#' @rdname fuzzy_bf-methods
#' @aliases fuzzy_bf,ScaledBernsteinFunction,ANY-method
#'
#' @seealso [rmo::ScaledBernsteinFunction-class]
#' @examples
#' bf <- fuzzy_bf(rmo::ScaledBernsteinFunction(
#'  scale = 1, original = rmo::LinearBernsteinFunction()
#' ))
#' @importFrom methods setMethod validObject
#' @importFrom rmo ScaledBernsteinFunction
#' @importFrom stats rexp
#' @export
setMethod(
  "fuzzy_bf", "ScaledBernsteinFunction",
  function(bf) {
    bf@scale <- stats::rexp(1)
    bf@original <- fuzzy_bf(bf@original)
    validObject(bf)

    bf
  }
)

#' @rdname fuzzy_bf-methods
#' @aliases fuzzy_bf,SumOfBernsteinFunctions,ANY-method
#'
#' @seealso [rmo::SumOfBernsteinFunctions-class]
#' @examples
#' bf <- fuzzy_bf(rmo::SumOfBernsteinFunctions(
#'  first = rmo::ConstantBernsteinFunction(),
#'  second = rmo::LinearBernsteinFunction()
#' ))
#' @importFrom methods setMethod validObject
#' @importFrom rmo SumOfBernsteinFunctions
#' @export
setMethod(
  "fuzzy_bf", "SumOfBernsteinFunctions",
  function(bf) {
    bf@first <- fuzzy_bf(bf@first)
    bf@second <- fuzzy_bf(bf@second)
    validObject(bf)

    bf
  }
)
