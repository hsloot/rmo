#' @importFrom methods new setClass setValidity setGeneric setMethod validObject
#'    callNextMethod show classLabel
NULL

#' Bernstein functions
#'
#' A virtual superclass for all implementations of the various classes of *Bernstein functions*.
#'
#' @details
#'
#' A *Bernstein function* is a non-negative, non-decreasing, infinitely often differentiable
#' function with whose recursive finite forward differences have alternating signs:
#' \deqn{
#'     {(-1)}^{i-1} \Delta^{i}{ \psi{(x)} }
#'         \geq 0 ,
#'             \quad \forall  i \in \mathbb{N}, x \geq 0 .
#' }
#' In particular, the following sequence defines an *extendible Marshall–Olkin distribution*:
#' \deqn{
#'     {(-1)}^{i-1} \Delta^{i}{ \psi{(d-i)} } ,
#'         \quad 1 \leq i \leq d .
#' }
#'
#' @references
#'   \insertRef{Schilling2012a}{rmo}
#'
#' @seealso [LevyBernsteinFunction-class], [ScaledBernsteinFunction-class],
#' [SumOfBernsteinFunctions-class], [CompositeScaledBernsteinFunction-class],
#' [CompleteBernsteinFunction-class], [ConstantBernsteinFunction-class],
#' [LinearBernsteinFunction-class], [PoissonBernsteinFunction-class],
#' [AlphaStableBernsteinFunction-class], [ExponentialBernsteinFunction-class],
#' [InverseGaussianBernsteinFunction-class], [GammaBernsteinFunction-class],
#' [ParetoBernsteinFunction-class]
#'
#' @export
setClass("BernsteinFunction", # nolint
    contains = "VIRTUAL")


#' Class for the \emph{linear Bernstein function}
#'
#' @slot scale The non-negative \emph{drift} parameter
#'   (i.e. \eqn{b} in the representation)
#'
#' @description
#' \emph{A linear Bernstein function} is a Bernstein function with only a drift,
#' i.e. \eqn{a = 0} and \eqn{\nu = 0}. In particular,
#' \deqn{
#'  \psi(x) = b x, x > 0.
#' }
#'
#' @seealso [BernsteinFunction-class],
#'   [valueOf()]
#'
#' @export LinearBernsteinFunction
LinearBernsteinFunction <- setClass("LinearBernsteinFunction", # nolint
    contains = "BernsteinFunction",
    slots = c(scale = "numeric"))

#' Class for the \emph{constant Bernstein function}
#'
#' @slot constant The non-negative \emph{killing} parameter (i.e. \eqn{a}
#'   in the representation)
#'
#' @description
#' A \emph{constant Bernstein function} is a Bernstein function with only a
#' constant part (for \eqn{x > 0}), i.e. \eqn{b = 0} and \eqn{\nu = 0}. In
#' particular,
#' \deqn{
#'   \psi(x) = a , x > 0
#' }
#'
#' @seealso [BernsteinFunction-class],
#'   [valueOf()]
#'
#' @export ConstantBernsteinFunction
ConstantBernsteinFunction <- setClass("ConstantBernsteinFunction", # nolint
    contains = "BernsteinFunction",
    slots = c(constant = "numeric"))


#' Class for \emph{scaled Bernstein functions}
#'
#' Berstein functions are stable under (non-negative) scalar multiplication,
#' i.e. if \eqn{\psi} is a Bernstein function and \eqn{\lambda \geq 0} , then
#' \deqn{
#'   x \mapsto \lambda \psi(x), x > 0,
#' }
#' is also a Bernstein function.
#'
#' @slot scale The scalar factor with which the original Bernstein function
#'   is to be multiplied.
#' @slot original The original Bernstein function which is to be multiplied.
#'
#' @seealso [BernsteinFunction-class],
#'   [valueOf()]
#'
#' @export ScaledBernsteinFunction
ScaledBernsteinFunction <- setClass("ScaledBernsteinFunction", # nolint
    contains = "BernsteinFunction",
    slots = c(scale = "numeric", original = "BernsteinFunction"))


#' Class for *input scaled Bernstein functions*
#'


#' Class for the *sum of two Bernstein functions*
#'
#' Bernstein functions are stable under addition, i.e. if \eqn{\psi_1} and
#' \eqn{\psi_2} are two Bernstein functions, then
#' \deqn{
#'   x \mapsto \psi_1(x) + \psi_2(x) , x>0,
#' }
#' is also a Bernstein function.
#'
#' @slot first The first summand (derived from [BernsteinFunction-class]).
#' @slot second The second summand (derived from [BernsteinFunction-class]).
#'
#' @seealso [BernsteinFunction-class],
#'   [valueOf()]
#'
#' @export SumOfBernsteinFunctions
SumOfBernsteinFunctions <- setClass("SumOfBernsteinFunctions", # nolint
    contains = "BernsteinFunction",
    slots = c(first = "BernsteinFunction", second = "BernsteinFunction"))

#' Class for the *composite scaled Bernstein function*
#'
#' Bernstein functions are stable under composition, i.e. if \eqn{\psi} is
#' a Bernstein function and `c > 0`, then
#' \deqn{
#'   x \mapsto \psi(c x)
#' }
#' is also a Bernstein function.
#'
#' @slot cscale The scale of the inner linear Bernstein function of the
#'   composition.
#' @slot original The original Bernstein function.
#'
#' @seealso [BernsteinFunction-class],
#'   [valueOf()]
#'
#' @export CompositeScaledBernsteinFunction
CompositeScaledBernsteinFunction <- setClass("CompositeScaledBernsteinFunction", # nolint
    contains = "BernsteinFunction",
    slots = c(cscale = "numeric", original = "BernsteinFunction"))


#' Virtual Class `LevyBernsteinFunction` for Levy Bernstein Functions
#'
#' A virtual superclass for all Bernstein functions which can representated
#' by a Lévy density (no drift or killing rate). That means that there exists
#' a Lévy measure \eqn{\nu} such that
#' \deqn{
#'   \psi(x) = \int_0^\infty (1 - e^{-ux}) \nu(du) , x > 0 .
#' }
#'
#' @seealso [BernsteinFunction-class], [valueOf()]
#'
#' @export
setClass("LevyBernsteinFunction",
    contains = c("BernsteinFunction", "VIRTUAL"))

#' Virtual Class `CompleteBernsteinFunction` for Complete Bernstein Functions
#'
#' A virtual superclass for all Bernstein functions which can representated
#' by a Stieltjes density (no drift or killing rate). That means that there exists
#' a Stieltjes measure \eqn{\sigma} such that
#' \deqn{
#'   \psi(x) = \int_0^\infty \frac{x}{x + u} \sigma(du) , x > 0 .
#' }
#'
#' @seealso [LevyBernsteinFunction-class],
#'   [BernsteinFunction-class]
#'   [valueOf()]
#'
#' @export
setClass("CompleteBernsteinFunction",
    contains = c("LevyBernsteinFunction", "VIRTUAL"))


#' Class for the \emph{Poisson Bernstein function}
#'
#' @slot lambda The (positive) arrival rate of the underlying Poisson process.
#' @slot eta The fixed (positive) jump size of the Poisson process.
#'
#' @description
#' The Poisson process with arrival-rate \eqn{\lambda} and fixed jump size
#' \eqn{\eta} is a Lévy subordinator corresponding to the Bernstein function
#' \deqn{
#'   \psi(x) = 1 - e^{-x\eta}, x>0.
#' }
#'
#' @details
#' For the Poisson Bernstein function, the higher-order alternatig iterated
#' foward differences can be calculated in closed form:
#' \deqn{
#'   {(-1)}^{k-1} \Delta^k \psi(x) = e^{-u\eta} (1-e^{-\eta})^k, x>0, k>0.
#' }
#'
#' @seealso [BernsteinFunction-class], [LevyBernsteinFunction-class]
#'   [valueOf()]
#'
#' @export PoissonBernsteinFunction
PoissonBernsteinFunction <- setClass("PoissonBernsteinFunction", # nolint
    contains = "LevyBernsteinFunction",
    slots = c(eta = "numeric"))


#' Class for the *\eqn{\alpha}-stable Bernstein function*
#'
#' @slot alpha The index \eqn{\alpha}.
#'
#' @description
#' For the \eqn{\alpha}-stable Lévy subordinator with \eqn{0 < \alpha < 1},
#' the corresponding Bernstein function is the power function with exponent
#' \eqn{\alpha}, i.e.
#' \deqn{
#'   \psi(x) = x^\alpha, \quad x>0.
#' }
#'
#' @details
#' For the \eqn{\alpha}-stable Bernstein function, the higher order alternating
#' iterated forward differences are known in closed form but cannot be evaluated
#' numerically without the danger of loss of significance. But we can use
#' numerical integration (here: [stats::integrate()]) to approximate it with the
#' following representation:
#' \deqn{
#'   {(-1)}^{k-1} \Delta^k \psi(x)
#'    = \int_0^\infty e^{-ux} (1-e^{-u})^k
#'      \alpha \frac{1}{\Gamma(1-\alpha) u^{1+\alpha}} du, x>0, k>0 .
#' }
#'
#' This Bernstein function is no. 1 in the list of complete Bernstein functions
#' in Chp. 16 of \insertCite{Schilling2012a}{rmo}.
#'
#' @references
#'   \insertAllCited{}
#'
#' @seealso [BernsteinFunction-class], [LevyBernsteinFunction-class],
#'   [CompleteBernsteinFunction-class],
#'   [valueOf()]
#'
#' @export AlphaStableBernsteinFunction
AlphaStableBernsteinFunction <- setClass("AlphaStableBernsteinFunction", # nolint
    contains = "CompleteBernsteinFunction",
    slots = c(alpha = "numeric"))


#' Class for the \emph{Inverse Gaussian Bernstein function}
#'
#' @slot eta The distribution parameter (drift of the
#'   underlying Gaussian process)
#'
#' @description
#' For the inverse Gaussian Lévy subordinator with \eqn{\eta > 0},
#' the corresponding Bernstein function is the function
#' \deqn{
#'   \psi(x) = \sqrt{2x + \eta^2} - \eta, x>0.
#' }
#'
#' @details
#' For the inverse Gaussian Bernstein function, the higher-order alternating
#' iterated forward differences are not known in closed-form, but
#' we can use numerical integration (here: [stats::integrate()])
#' to approximate it with the following representation:
#' \deqn{
#'  {(-1)}^{k-1} \Delta^{k} \psi(x)
#'    = \int_0^\infty e^{-ux} (1-e^{-u})^k \frac{1}{\sqrt{2\pi}
#'      u^{3/2}} e^{-\frac{1}{2}\eta^2 u} du, x>0, k>0.
#' }
#'
#' This Bernstein function can be found on p. 309 in \insertCite{Mai2017a}{rmo}.
#' Furthermore it is  a transformation of no. 2 in the list of complete Bernstein
#' functions in Chp. 16 of \insertCite{Schilling2012a}{rmo}.
#'
#' @references
#'  \insertAllCited{}
#'
#' @seealso [BernsteinFunction-class], [LevyBernsteinFunction-class],
#'   [valueOf()]
#'
#' @export InverseGaussianBernsteinFunction
InverseGaussianBernsteinFunction <- setClass("InverseGaussianBernsteinFunction", # nolint
    contains = "CompleteBernsteinFunction",
    slots = c(eta = "numeric"))


#' Class for the Exponential jump CPP Bernstein function
#'
#' @slot lambda The index \eqn{\lambda}.
#'
#' @description
#' For the Exponential jump CPP subordinator with \eqn{\lambda > 0},
#' the corresponding Bernstein function is
#' \deqn{
#'   \psi(x) = \frac{x}{x + \lambda}, x>0.
#' }
#'
#' @details
#' For the Exponential jump CPP Bernstein function, the higher order
#' alternating iterated forward differences are known in closed form:
#' \deqn{
#'   {(-1)}^{k-1} \Delta^k \psi(x)
#'    = \lambda \cdot B(k+1, x+\lambda), x>0, k>0 .
#' }
#'
#' This Bernstein function is no. 4 in the list of complete Bernstein functions
#' in Chp. 16 of \insertCite{Schilling2012a}{rmo}.
#'
#' @references
#'   \insertAllCited{}
#'
#' @seealso [BernsteinFunction-class], [LevyBernsteinFunction-class],
#'   [CompleteBernsteinFunction-class], [valueOf()]
#'
#' @export ExponentialBernsteinFunction
ExponentialBernsteinFunction <- setClass("ExponentialBernsteinFunction", # nolint
    contains = "CompleteBernsteinFunction",
    slots = c("lambda" = "numeric"))


#' Class for the \emph{Gamma Bernstein function}
#'
#' @slot a Scale parameter for the Lévy measure.
#'
#' @description
#' The \emph{Gamma Bernstein function}, is the Bernstein function of a
#' subordinator with a (scaled) Gamma distribution. The representation is for
#' \eqn{a > 0}
#' \deqn{
#'   \psi(x) = \log(1 + \frac{x}{a}), x > 0.
#' }
#'
#' @details
#' For this Bernstein function, the higher-order alternating iterated forward
#' differences are known in closed form but cannot be evaluated numerically
#' without the danger of loss of significance. But we can use numerical
#' integration (here: [stats::integrate()]) to approximate it with the
#' following representation:
#' \deqn{
#'   {(-1)}^{k-1} \Delta^{k} \psi(x)
#'     = \int_{0}^{\infty} e^{-ux} {(1 - e^{-u})}^{k}
#'       \frac{e^{-au}}{u} du, x>0, k>0.
#' }
#'
#' This Bernstein function is no. 26 in the list of complete Bernstein functions
#' in Chp. 16 of \insertCite{Schilling2012a}{rmo}.
#'
#' @references
#'   \insertAllCited{}
#'
#' @seealso [BernsteinFunction-class], [LevyBernsteinFunction-class],
#'   [CompleteBernsteinFunction-class], [valueOf()]
#'
#' @export GammaBernsteinFunction
GammaBernsteinFunction <- setClass("GammaBernsteinFunction", # nolint
    contains = "CompleteBernsteinFunction",
    slots = c(a = "numeric"))


#' Class for the \emph{Pareto Bernstein function}
#'
#' @slot alpha The index \eqn{\alpha}
#' @slot x0 The cutoff point \eqn{x_0}
#'
#' @description
#' For the Pareto-jump compound Poisson process with index \eqn{0 < \alpha < 1}
#' and cutoff point \eqn{x0}, the corresponding Bernstein function is
#' \deqn{
#'   \psi(x)
#'   = 1 - e^{-x x_0} + (x_0 x)^\alpha \Gamma(1-\alpha, x_0 x) ,
#'   x>0 .
#' }
#'
#' @details
#' For this Bernstein function, the higher-order alternating iterated forward
#' differences are known in closed form but cannot be evaluated numerically
#' without the danger of loss of significance. But we can use numerical
#' integration (here: [stats::integrate()]) to approximate it with the following
#' representation:
#' \deqn{
#'   {(-1)}^{k-1} \Delta^k \psi(x)
#'   = \int_{x_0}^\infty e^{-ux} (1-e^{-u})^k
#'   \alpha \frac{{x_0}^\alpha}{t^{1+\alpha}} du,
#'   x>0, k>0 .
#' }
#'
#' The Pareto Bernstein function, in combination with a linear Bernstein
#' function can be used to approximate the Bernstein function of an
#' \eqn{\alpha}-stable Subordinator, see Sec. 5.3 of
#' \insertCite{Fernandez2015a}{rmo}.
#'
#' @references
#'   \insertAllCited{}
#'
#' @seealso [BernsteinFunction-class], [LevyBernsteinFunction-class],
#'   [valueOf()]
#'
#' @export ParetoBernsteinFunction
ParetoBernsteinFunction <- setClass("ParetoBernsteinFunction", # nolint
    contains = "LevyBernsteinFunction",
    slots = c(alpha = "numeric", x0 = "numeric"))
