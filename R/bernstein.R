# #### Virtual classes of Bernstein functions ####

#' Virtual Class \code{BernsteinFunction} for Bernstein Functions
#'
#' A virtual superclass for all implementations of the various classes of
#' Bernstein functions which have a representaion
#' \deqn{
#'  \psi(x) = a + b x + \int_0^\infty (1 - e^{-ux}) \nu(du) , x > 0
#' }
#' for non-negative constants \eqn{a, b \geq 0}, called \emph{killing rate}
#' and \emph{drift}, and a \emph{Lévy measure}
#' \eqn{\nu} on \eqn{(0, \infty)}.
#'
#' Bernstein functions have a 1-1 relationship to Lévy subordinators s.t.
#' for every Bernstein function \eqn{\psi}, there exists a Lévy subordinator
#' \eqn{\Lambda} with
#' \deqn{
#'   E[e^{-x \Lambda_t}]
#'     = e^{-t \psi(x)} , t, x > 0 .
#' }
#' \itemize{
#'   \item \eqn{a} Describes the (exponential) \emph{killing rate}, which
#'     determines the rate of an exponentially distributed event that sends
#'     the subordinator to infinity.
#'   \item \eqn{b} Describes the linear \emph{drift} of the Lévy subordinator.
#'   \item \eqn{\nu} determines the arrival-rate and shock-size distribution
#'     for the jumps of the Lévy subordinator.
#' }
#'
#' For a theoretic treatment of Bernstein function, we refer to
#' \insertCite{Schilling2012a}{rmo}.
#'
#' @references
#'   \insertAllCited{}
#'
#' @seealso \linkS4class{ConstantBernsteinFunction},
#'   \linkS4class{LinearBernsteinFunction},
#'   \linkS4class{PoissonBernsteinFunction},
#'   \linkS4class{AlphaStableBernsteinFunction},
#'   \linkS4class{ScaledBernsteinFunction},
#'   \linkS4class{SumOfBernsteinFunctions}
#' @importFrom methods new setClass
setClass("BernsteinFunction", # nolint
  contains = "VIRTUAL")



# #### Generic function for `valueOf` ####

#' Returns values for Bernstein functions
#'
#' @description
#' This method allows you to calculate the values for a Bernstein function
#' and its higher-order, alternating iterated forward differences, i.e.
#' \deqn{
#'   {(-1)}^{k} \Delta^k \psi(x), x > 0.
#' }
#'
#' @param object The Bernstein function object.
#' @param x The value at which the Bernstein function is to be evaluated.
#' @param difference_order The order of the alternating iterated forward
#'   differences taken on the Bernstein function (\eqn{k} in
#'   the representation).
#'
#' @docType methods
#' @rdname valueOf-methods
#'
#' @seealso \linkS4class{BernsteinFunction}
#'
#' @export
setGeneric("valueOf",
  def=function(object, x, difference_order) {
    standardGeneric("valueOf")
  })



# #### Linear and constant Bernstein function ####

#' Class for the \emph{linear Bernstein function}
#'
#' @examples
#' bf <- LinearBernsteinFunction(scale = 2)
#'
#' @slot scale The non-negative \emph{drift} parameter
#'   (i.e. \eqn{b} in the representation)
#'
#' @description
#' \emph{A linear Bernstein function} is a Bernstein function with only
#' a drift, i.e. \eqn{a = 0} and \eqn{\nu = 0}.
#' In particular,
#' \deqn{
#'  \psi(x) = b x, x > 0.
#' }
#'
#' @seealso \linkS4class{BernsteinFunction}
#'
#' @importFrom methods new setClass
#' @include assert.R
#'
#' @export LinearBernsteinFunction
LinearBernsteinFunction <- setClass("LinearBernsteinFunction", # nolint
  contains = "BernsteinFunction",
  slots = c(scale = "numeric"),
  validity = function(object) {
    if (!is_nonnegative_number(object@scale)) {
      return("scale must be non-negative number")
    }

    TRUE
  })

#' @rdname valueOf-methods
#' @aliases valueOf,LinearBernsteinFunction,numeric,integer,ANY-method
#'
#' @seealso \linkS4class{LinearBernsteinFunction}
#'
#' @importFrom methods setMethod
#' @include assert.R
#' 
#' @export
setMethod("valueOf",
  signature = c("LinearBernsteinFunction", "numeric", "integer"),
  definition = function(object, x, difference_order = 0) {
    assert_that(is_nonnegative_number(difference_order),
      is_nonnegative_vector(x))

    if (difference_order == 0)
      return(object@scale * x)
    else if (difference_order == 1)
      return(rep(object@scale, length(x)))
    else
      return(rep(0, length(x)))
  })


#' Class for the \emph{constant Bernstein function}
#'
#' @examples
#' bf <- ConstantBernsteinFunction(constant = 0.2)
#'
#' @slot constant The non-negative \emph{killing} parameter (i.e. \eqn{a}
#'   in the representation)
#'
#' @description
#' A \emph{constant Bernstein function} is a Bernstein function with only a
#' constant part (for \eqn{x > 0}), i.e. \eqn{b = 0} and \eqn{\nu = 0}.
#' In particular,
#' \deqn{
#'   \psi(x) = a , x > 0
#' }
#'
#' @seealso \linkS4class{BernsteinFunction}
#'
#' @importFrom methods new setClass
#' @include assert.R
#'
#' @export ConstantBernsteinFunction
ConstantBernsteinFunction <- setClass("ConstantBernsteinFunction", # nolint
  contains = "BernsteinFunction",
  slots = c(constant = "numeric"),
  validity = function(object) {
    if (!is_nonnegative_number(object@constant)) {
      return("constant must be non-negative number")
    }

    TRUE
  })

#' @rdname valueOf-methods
#' @aliases valueOf,ConstantBernsteinFunction,numeric,integer,ANY-method
#'
#' @seealso \linkS4class{ConstantBernsteinFunction}
#'
#' @importFrom methods setMethod
#' @include assert.R
#'
#' @export
setMethod("valueOf", # nolint
  signature = c("ConstantBernsteinFunction", "numeric", "integer"),
  definition = function(object, x, difference_order = 0) {
    assert_that(is_nonnegative_number(difference_order),
      is_nonnegative_vector(x))

    if (difference_order == 0) {
      return(ifelse(x == 0, 0, object@constant))
    } else {
      return(ifelse(x == 0, object@constant, 0))
    }
  })



# #### Classes for operations on Bernstein functions ####

#' Class for \emph{scaled Bernstein functions}
#'
#' @examples
#' original_bf <- AlphaStableBernsteinFunction(alpha=0.5)
#' bf <- ScaledBernsteinFunction(scale=2, original=original_bf)
#'
#' @slot scale The scalar factor with which the original Bernstein function
#'   is to be multiplied.
#' @slot original The original Bernstein function which is to be multiplied.
#'
#' @description
#' Berstein functions are stable under (non-negative) scalar multiplication,
#' i.e. if \eqn{\psi} is a Bernstein function and \eqn{\lambda \geq 0} , then
#' \deqn{
#'   x \mapsto \lambda \psi(x), x > 0,
#' }
#' is also a Bernstein function.
#'
#' @seealso \linkS4class{BernsteinFunction}
#'
#' @importFrom methods new setClass
#' @include assert.R
#'
#' @export ScaledBernsteinFunction
ScaledBernsteinFunction <- setClass("ScaledBernsteinFunction", # nolint
  contains = "BernsteinFunction",
  slots = c(scale = "numeric", original = "BernsteinFunction"),
  validity = function(object) {
    if (!is_nonnegative_number(object@scale)) {
      return("scale must be non-negative number")
    }

    TRUE
  })

#' @rdname valueOf-methods
#' @aliases valueOf,ScaledBernsteinFunction,numeric,integer,ANY-method
#'
#' @seealso \linkS4class{ScaledBernsteinFunction}
#'
#' @importFrom methods setMethod
#' @include assert.R
#'
#' @export
setMethod("valueOf",
  signature = c("ScaledBernsteinFunction", "numeric", "integer"),
  definition = function(object, x, difference_order=0) {
    object@scale * valueOf(object@original, x, difference_order)
  })


#' Class for the \emph{sum of two Bernstein functions}
#'
#' @examples
#' first_bf <- LinearBernsteinFunction(scale=0.2)
#' second_bf <- AlphaStableBernsteinFunction(alpha=0.5)
#' bf <- SumOfBernsteinFunctions(first=first_bf, second=second_bf)
#'
#' @slot first The first summand (Bernstein function).
#' @slot second The second summand (Bernstein function).
#'
#' @description
#' Bernstein functions are stable under addition, i.e. if \eqn{\psi_1}
#' and \eqn{\psi_2} are two Bernstein functions, then
#' \deqn{
#'   x \mapsto \psi_1(x) + \psi_2(x) , x>0,
#' }
#' is also a Bernstein function.
#'
#' @seealso \linkS4class{BernsteinFunction}
#'
#' @importFrom methods new setClass
#'
#' @export SumOfBernsteinFunctions
SumOfBernsteinFunctions <- setClass("SumOfBernsteinFunctions", # nolint
  contains = "BernsteinFunction",
  slots = c(first = "BernsteinFunction", second = "BernsteinFunction"))

#' @rdname valueOf-methods
#' @aliases valueOf,SumOfBernsteinFunctions,numeric,integer,ANY-method
#'
#' @seealso \linkS4class{SumOfBernsteinFunctions}
#'
#' @importFrom methods setMethod
#' @include assert.R
#'
#' @export
setMethod("valueOf",
  signature = c("SumOfBernsteinFunctions", "numeric", "integer"),
  definition = function(object, x, difference_order=0) {
    valueOf(object@first, x, difference_order) +
      valueOf(object@second, x, difference_order)
  })



# #### The Poisson Bernstein functions ####

#' Class for the \emph{Poisson Bernstein function}
#'
#' @examples
#' bf <- PoissonBernsteinFunction(lambda=0.2, eta=2)
#'
#' @slot lambda The (positive) arrival rate of the underlying Poisson process.
#' @slot eta The fixed (positive) jump size of the Poisson process.
#'
#' @description
#' The Poisson process with arrival-rate \eqn{\lambda} and fixed jump size
#' \eqn{\eta} is a Lévy subordinator corresponding to the Bernstein function
#' \deqn{
#'   \psi(x) = \lambda (1 - e^{-x\eta}) , x>0.
#' }
#'
#' @details
#' For the Poisson Bernstein function, the higher-order alternatig iterated
#' foward differences can be calculated in closed form:
#' \deqn{
#'   {(-1)}^{k-1} \Delta^k \psi(x) = e^{-u\eta} (1-e^{-\eta})^k, x>0, k>0.
#' }
#'
#' @seealso \linkS4class{BernsteinFunction}
#'
#' @importFrom methods new setClass
#' @include assert.R
#'
#' @export PoissonBernsteinFunction
PoissonBernsteinFunction <- setClass("PoissonBernsteinFunction", # nolint
  contains = "BernsteinFunction",
  slots = c("lambda", "eta"),
  validity = function(object) {
    if (!is_nonnegative_number(object@lambda) ||
          !is_nonnegative_number(object@eta)) {
      return("lambda and eta must be positive numbers")
    }

    TRUE
  })

#' @rdname valueOf-methods
#' @aliases valueOf,PoissonBernsteinFunction,numeric,integer,ANY-method
#'
#' @seealso \linkS4class{PoissonBernsteinFunction}
#'
#' @importFrom methods setMethod
#' @include assert.R
#'
#' @export
setMethod("valueOf",
  signature = c("PoissonBernsteinFunction", "numeric", "integer"),
  definition = function(object, x, difference_order=0) {
    assert_that(is_nonnegative_number(difference_order),
      is_nonnegative_vector(x))

    if (difference_order == 0) {
      return(object@lambda * (1 - exp(-x * object@eta)))
    } else {
      return(object@lambda * exp(-x * object@eta) *
        (1 - exp(-object@eta))^difference_order)
    }
  })



# #### Algebraic Bernstein functions ####

#' Class for the \emph{\eqn{\alpha}-stable Bernstein function}
#'
#' @examples
#' bf <- AlphaStableBernsteinFunction(alpha=0.5)
#'
#' @slot alpha The index \eqn{\alpha}.
#'
#' @description
#' For the \eqn{\alpha}-stable Lévy subordinator with \eqn{0 < \alpha < 1},
#' the corresponding Bernstein function is the power function with exponent
#' \eqn{\alpha}, i.e.
#' \deqn{
#'   \psi(x) = x^\alpha, x>0.
#' }
#'
#' @details
#' For the \eqn{\alpha}-stable Bernstein function, the higher order alternating
#' iterated forward differences are known in closed form but cannot be evaluated
#' numerically without the danger of loss of significance. But we can use
#' numerical integration (here: \code{\link[stats]{integrate}}) to approximate
#' it with the following representation:
#' \deqn{
#'   {(-1)}^{k-1} \Delta^k \psi(x)
#'    = \int_0^\infty e^{-ux} (1-e^{-u})^k
#'      \alpha / \Gamma(1-\alpha) u^{-1-\alpha} du, x>0, k>0 .
#' }
#'
#' This Bernstein function is no. 1 in the list of complete Bernstein functions
#' in Chp. 16 of \insertCite{Schilling2012a}{rmo}.
#'
#' @references
#'   \insertAllCited{}
#'
#' @seealso \linkS4class{BernsteinFunction}
#'
#' @importFrom methods new setClass
#' @importFrom assertthat is.number
#' @include assert.R
#'
#' @export AlphaStableBernsteinFunction
AlphaStableBernsteinFunction <- setClass("AlphaStableBernsteinFunction", # nolint
  contains = "BernsteinFunction",
  slots = c("alpha"),
  validity = function(object) {
    if (!is_positive_number(object@alpha) || object@alpha >= 1) {
      return("alpha must be number between 0 and 1")
    }

    TRUE
  })

#' @rdname valueOf-methods
#' @aliases valueOf,AlphaStableBernsteinFunction,numeric,integer,ANY-method
#'
#' @seealso \linkS4class{AlphaStableBernsteinFunction}
#'
#' @importFrom methods setMethod
#' @importFrom stats integrate
#' @include assert.R
#'
#' @export
setMethod("valueOf",
  signature = c("AlphaStableBernsteinFunction", "numeric", "integer"),
  definition = function(object, x, difference_order=0) {
    assert_that(is_nonnegative_number(difference_order),
      is_nonnegative_vector(x))

    if (difference_order > 0) {
      sapply(x, function(y) integrate(
        f=function(u) {
          exp(-y * u) * (1 - exp(-u))^difference_order *
            object@alpha / gamma(1-object@alpha) * u ^ (-1-object@alpha) ## Lévy density
        }, lower = 0, upper = Inf)$value)
    } else {
      x^object@alpha
    }
  })



# #### Logarithmic Bernstein functions ####

#' Class for the \emph{Gamma Bernstein function}
#'
#' @examples
#' bf <- GammaBernsteinFunction(a=2)
#'
#' @slot a Scale parameter for the Lévy measure.
#'
#' @description
#' The \emph{Gamma Bernstein function}, is the Bernstein function of a
#' subordinator with a (scaled) Gamma distribution. The representation is
#' for \eqn{a > 0}
#' \deqn{
#'   \psi(x) = \log(1 + \frac{x}{a}), x > 0.
#' }
#'
#' @details
#' For this Bernstein function, the higher-order alternating iterated forward
#' differences are known in closed form but cannot be evaluated numerically
#' without the danger of loss of significance. But we can use numerical
#' integration (here: \code{\link[stats]{integrate}}) to approximate it with the
#' following representation:
#' \deqn{
#'   {(-1)}^{k-1} \Delta^{k} \psi(x)
#'     = \int_{0}^{\infty} e^{-ux} {(1 - e^{-u})}^{k}
#'       e^{-au} / u du, x>0, k>0.
#' }
#'
#' This Bernstein function is no. 26 in the list of complete Bernstein functions
#' in Chp. 16 of \insertCite{Schilling2012a}{rmo}.
#'
#' @references
#'   \insertAllCited{}
#'
#' @seealso \linkS4class{BernsteinFunction}
#'
#' @importFrom methods new setClass
#' @include assert.R
#'
#' @export GammaBernsteinFunction
GammaBernsteinFunction <- setClass("GammaBernsteinFunction", # nolint
  contains="BernsteinFunction",
  slots=c(a = "numeric"),
  validity = function(object) {
    if (!is_positive_number(object@a)) {
      return("a must be positive number")
    }

    TRUE
  })

#' @rdname valueOf-methods
#' @aliases valueOf,GammaBernsteinFunction,numeric,integer,ANY-method
#'
#' @seealso \linkS4class{GammaBernsteinFunction}
#'
#' @importFrom methods setMethod
#' @importFrom stats integrate
#' @include assert.R
#'
#' @export
setMethod("valueOf",
  signature = c("GammaBernsteinFunction", "numeric", "integer"),
  definition = function(object, x, difference_order=0) {
    assert_that(is_nonnegative_number(difference_order),
      is_nonnegative_vector(x))

    if (difference_order > 0) {
      sapply(x, function(y) integrate(
        f=function(u) {
          exp(-y*u) * (1 - exp(-u))^difference_order *
            exp(-object@a*u)/u ## Lévy density
        }, lower = 0, upper = Inf)$value)
    } else {
      log(1 + x/object@a)
    }
  })
