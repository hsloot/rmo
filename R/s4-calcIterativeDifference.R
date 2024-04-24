#' Evaluate Bernstein Function differences
#'
#' Calculate the values for a Bernstein function and its higher-order,
#' alternating iterated forward differences, possibly scaled by a binomial
#' coefficient, i.e.,
#' \deqn{
#'   \binom{n}{k} {(-1)}^{j-1} \Delta^{j}{ \psi(c x) } ,
#'     \quad x > 0 .
#' }
#' The evaluation of Bernstein functions using this formula is usually not
#' numerically stable. Consequently, the various alternative approaches are used
#' dependent on the class of the Bernstein function.
#'
#' @inheritParams valueOf0
#' @param difference_order A nonnegative integer with the order of the
#'   alternating iterated forward differences taken on the Bernstein function.
#' @param n,k Nonnegative numbers for the binomial factor.
#' @param ... Pass-through parameter.
#'
#' @importFrom methods setGeneric
#' @family Bernstein function generics
#' @export
#' @examples
#' bf <- AlphaStableBernsteinFunction(alpha = 0.7)
#' calcIterativeDifference(bf, 1:5)
setGeneric(
  "calcIterativeDifference",
  function(object, x, difference_order = 0L, n = 1L, k = 0L, cscale = 1, ...) { # nolint
    standardGeneric("calcIterativeDifference")
  }
)
