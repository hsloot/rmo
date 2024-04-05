#' @importFrom methods new setClass setValidity setMethod validObject
#'   callNextMethod show classLabel
NULL

#' Virtual superclass for Bernstein functions
#'
#' A virtual superclass for all implementations of the various classes of
#' *Bernstein functions*.
#'
#' @details
#'
#' A *Bernstein function* is a nonnegative, nondecreasing, infinitely often
#' differentiable function with whose recursive finite forward differences have
#' alternating signs:
#' \deqn{
#'     {(-1)}^{i-1} \Delta^{i}{ \psi{(x)} }
#'         \geq 0 ,
#'             \quad \forall  i \in \mathbb{N}, x \geq 0 .
#' }
#'
#' ### Extendible Marshall--Olkin distributions
#'
#' A Bernstein function an *extendible Marshall–Olkin distribution* with
#' *exchangeable shock-size arrival intensities*
#' \deqn{
#'   \eta_{i}
#'     = \binom{d}{i} {(-1)}^{i-1} \Delta^{i}{ \psi{(d-i)} } ,
#'          \quad 1 \leq i \leq d .
#' }
#'
#' The (upper triagonal) infinitesimal Markov generator of the associated
#' death-counting process is calculated recursively:
#' \deqn{
#'   q_{0, i}^\ast
#'     = \eta_{i} ,
#'       \quad i \in {\{ 1 , \ldots , d \}} ,
#' }
#' and
#' \deqn{
#'   q_{i+1, j+1}^\ast
#'     = \frac{d-j}{d-i} q_{i,j}^\ast + \frac{j+1-i}{d-i} q_{i, j+1}^\ast ,
#'      \quad 0 \leq i < j \leq d .
#' }
#'
#' @references
#'   \insertRef{Schilling2012a}{rmo}
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
