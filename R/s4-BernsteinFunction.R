#' @importFrom methods new setClass setValidity setMethod validObject
#'   callNextMethod show classLabel
NULL

#' Virtual superclass for Bernstein function objects
#'
#' @description
#' Bernstein functions parametrize extendible Marshall--Olkin distributions.
#' They are closed under addition, scalar multiplication, composite scalar
#' multiplication, and consequently also convex recombination:
#'
#' - Pass a Bernstein function object to [rextmo()] to simulate from the
#'   associated extendible MO distribution.
#' - Use [SumOfBernsteinFunctions-class] for adding two Bernstein functions,
#' - Use [ScaledBernsteinFunction-class] for scalar multiplication of a
#'   Bernstein function,
#' - Use [CompositeScaledBernsteinFunction-class] for composite scalar
#'   multiplication of a Bernstein function, and
#' - Use [ConvexCombinationOfBernsteinFunctions-class] for convex recombination
#'   of Bernstein functions.
#'
#' @details
#' ## Mathematical definition
#' A *Bernstein function* is a nonnegative, nondecreasing, infinitely often
#' differentiable function whose recursive finite forward differences have
#' alternating signs:
#' \deqn{
#'     {(-1)}^{i-1} \Delta^{i}{ \psi{(x)} }
#'         \geq 0 ,
#'             \quad \forall  i \in \mathbb{N}, x \geq 0 .
#' }
#'
#' ## Lévy-Khintchine representation
#' Bernstein functions have the so-called Lévy-Khintchine representation:
#' \deqn{
#'    \psi{(x)}
#'        = a x + b
#'          + \int_{0}^{\infty}{
#'              {\left[ 1 - e^{-x y} \right]} {\nu{(dy)}}
#'            } ,
#'          \quad x \geq 0 ,
#' }
#' for some nonnegative constants \eqn{a} and \eqn{b} and a Lévy measure
#' \eqn{\nu}. A Lévy measure is a measure on the Borel sets of the nonnegative
#' real line that satisfies the following properties:
#' \deqn{
#'   \int_{0}^{\infty}{
#'     \min{\{ 1 , y \}} {\nu{(dy)}} < \infty .
#'   }
#' }
#'
#' ## Lévy-frailty model and ext. MO distributions
#' Bernstein functions are uniquely linked to extendible Marshall--Olkin
#' distributions via the Lévy-frailty model.
#'
#' First, each Bernstein functions is uniquely linked to a Lévy subordinator via
#' the *Lévy-Khintchine representation*:
#' \deqn{
#'   \Lambda{(t)}
#'     = \begin{cases}
#'       b t + \Lambda_{\nu}{(t)} , & \text{if } t < \epsilon , \\
#'       \infty , & \text{otherwise} ,
#'     \end{cases}
#' }
#' where \eqn{\Lambda_{\nu}{(t)}} is the Lévy subordinator associated with the
#' Lévy measure \eqn{\nu} and \eqn{\epsilon} is an independently exponentially
#' distributed random variable with rate \eqn{a}. If \eqn{\nu} is a finite
#' measure, the Lévy subordinator is a compound Poisson process with intensity
#' \eqn{\nu{(0, \infty)}} and jump sizes \eqn{\nu{(dy)} / \nu{((0, \infty))} }.
#'
#' Second, the Lévy subordinator is unique linked to an
#' extendible Marshall--Olkin distribution via the stochastic representation:
#' \deqn{
#'   \tau_{i}
#'     = \inf{\left \{ t \geq 0 : \Lambda{(t)} > E_{i} \right \}} ,
#'       \quad 1 \leq i \leq d ,
#' }
#' for independently unit exponentially distributed random variables
#' \eqn{E_{i}}.
#'
#' ## Ext. MO parameter interpretation
#' The Lévy-frailty model motivates the following interpretation of the
#' parameters for ext. MO distributions:
#' - `a` is the killing rate,
#' - `b` is the drift,
#' - `gamma` is a scaling factor for the total shock-arrival intensity,
#' - `family` is the name of the pure-jump Lévy measure, and
#' - `eta` are the pure-jump family parameters.
#'
#' To understand the influence of these parameters on the extendible
#' Marshall–Olkin distribution's dependence properties, the following
#' considerations are helpful:
#'
#' - A pure-killing Bernstein function (i.e., \eqn{a > 0}, \eqn{b = 0}, and
#'   \eqn{\nu \equiv 0}) corresponds to complete comononotonicity.
#' - A pure-drift Bernstein function (i.e., \eqn{a = 0}, \eqn{b > 0}, and
#'   \eqn{\nu \equiv 0}) corresponds to independence.
#' - A pure-jump Bernstein function (i.e., \eqn{a = 0}, \eqn{b = 0}, and
#'   \eqn{\nu \not\equiv 0}) can model various dependence structures. However,
#'   larger jump intensities lead to weaker dependendence and larger jump sizes
#'   lead to stronger dependence.
#'
#' Consequently, weighting these cases with the parameters `a`, `b`, and `gamma`
#' allows for a flexible modeling of the dependence structure.
#'
#' ## Ext.MO marginal and dependence properties
#' For a given Bernstein function, the marginal rate and lower-tail dependence
#' coefficient of the associated extendible Marshall–Olkin distribution can be
#' calculated using as follows:
#' \deqn{
#'   \text{Marginal rate}
#'     = \psi{(1)}
#' }
#' and
#' \deqn{
#'   \text{LTDC}
#'     = 2 - \psi{(2)} / \psi{(1)} .
#' }
#'
#' ## Evaluating Bernstein functions
#' In context of extendible Marshall--Olkin distributions, the following
#' expression is frequently evaluated:
#' \deqn{
#'   \binom{n}{k} {(-1)}^{j-1} \Delta^{j}{ \psi{( c x )} } ,
#'     \quad 0 \leq k \leq n , j \in \mathbb{N}, x \geq 0 .
#' }
#' The evaluation of Bernstein functions using this formula is usually not
#' numerically stable. Consequently, the various alternative approaches are used
#' dependent on the class of the Bernstein function. Use the method [valueOf()]
#' to evaluate or approximate this expression for a given Bernstein function.
#'
#' ## Exchangeable Marshall–Olkin distributions
#' An alternative stochastic representation of an exchangeable Marshall–Olkin
#' distributions is given by the so-called *Markovian death-counting model*. It
#' defines the components' death times as randomized order statistics simulated
#' via the Markovian death-counting processes with infinitesimal generator
#' matrix:
#' \deqn{
#'   q_{i, j}^\ast
#'     = \binom{d-i}{j-i} \begin{cases}
#'       -\psi{(d-i)} , & \text{if } i = j , \\
#'       {(-1)}^{j-i-1} \Delta^{j-i}{ \psi{(d-i)} } , & \text{if } i < j , \\
#'       0 , & \text{otherwise} .
#'    \end{cases}
#' }
#' The evaluation of the infinitesimal generator matrix using this formula is
#' usually not numerically stable. Consequently, the various alternative
#' approaches are used dependent on the class of the Bernstein function. Use the
#' method [exQMatrix()] to evaluate or approximate this expression for a given
#' Bernstein function.
#'
#' For the *all-alive-state*, the generator's first row has the interpretation
#' of *exchangeable shock-size-arrival intensities*:
#' \deqn{
#'   \eta_{i}
#'     = \binom{d}{i} {(-1)}^{i-1} \Delta^{i}{ \psi{(d-i)} } ,
#'          \quad 1 \leq i \leq d .
#' }
#' As noted above, their evaluation is usually not numerically stable, and
#' various alternative approaches are used dependent on the class of the
#' Bernstein function. Use the method [exIntensities()] to evaluate or
#' approximate them.
#'
#' ## The Exogeneous shock model and the Arnold model
#' Another alternative stochastic representation of Marshall–Olkin distributions
#' is
#'
#' @references
#'   \insertRef{Schilling2012a}{rmo}
#'   \insertRef{Sloot2022a}{rmo}
#'
#' @seealso [valueOf()], [intensities()], [uexIntensities()], [exIntensities()],
#'   [exQMatrix()], [rextmo()], [rpextmo()]
#'
#' @docType class
#' @name BernsteinFunction-class
#' @rdname BernsteinFunction-class
#' @family Bernstein function classes
#' @family Virtual Bernstein function classes
#' @export
setClass("BernsteinFunction", # nolint
  contains = "VIRTUAL"
)

#' For S4 methods that require a documentation entry but only clutter the index.
#'
#' @name hidden_aliases
#' @rdname hidden_aliases
#' @usage NULL
#' @format NULL
#' @keywords internal
#' @docType methods
NULL


#' @include s4-valueOf0.R s4-valueOf.R
#' @importFrom methods setMethod
#' @keywords internal
setMethod(
  "valueOf0", "BernsteinFunction",
  function(object, x, cscale = 1, ...) {
    valueOf(
      object, x,
      difference_order = 0L, n = 1L, k = 0L, cscale = cscale, ...
    )
  }
)

#' @rdname hidden_aliases
#'
#' @inheritParams uexIntensities
#'
#' @include s4-uexIntensities.R s4-valueOf.R
#' @export
setMethod(
  "uexIntensities", "BernsteinFunction",
  function(object, d, cscale = 1, ...) {
    sapply(1:d, function(i) valueOf(object, d - i, i, cscale = cscale, ...))
  }
)

#' @rdname hidden_aliases
#'
#' @inheritParams exIntensities
#'
#' @include s4-exIntensities.R s4-valueOf0.R s4-valueOf.R
#' @export
setMethod(
  "exIntensities", "BernsteinFunction",
  function(object, d, cscale = 1, ...) {
    if (d == 2) {
      out <- d * (
        valueOf0(object, d, cscale = cscale) -
          valueOf0(object, d - 1, cscale = cscale)
      )
    } else {
      out <- c(
        d * (
             valueOf0(object, d, cscale = cscale) -
               valueOf0(object, d - 1, cscale = cscale)),
        sapply(
          2:(d - 1),
          function(i) {
            valueOf(
              object, d - i, i,
              n = d, k = i, cscale = cscale, ...
            )
          }
        )
      )
    }

    c(out, pmax(valueOf0(object, d, cscale = cscale) - sum(out), 0))
  }
)

#' @rdname hidden_aliases
#'
#' @inheritParams intensities
#'
#' @include  s4-intensities.R s4-uexIntensities.R RcppExports.R
#' @export
setMethod(
  "intensities", "BernsteinFunction",
  function(object, d, cscale = 1, ...) {
    uexi2i(uexIntensities(object, d, cscale = cscale, ...))
  }
)

#' @rdname hidden_aliases
#'
#' @inheritParams exQMatrix
#'
#' @include s4-exQMatrix.R s4-exIntensities.R RcppExports.R
#' @export
setMethod(
  "exQMatrix", "BernsteinFunction",
  function(object, d, cscale = 1, ...) {
    exi2exqm(exIntensities(object, d, cscale = cscale, ...))
  }
)
