#' Evaluate Bernstein Functions
#'
#' Calculate the values for a Bernstein function and its higher-order,
#' alternating iterated forward differences, possibly scaled by a binomial
#' coefficient, i.e.,
#' \deqn{
#'   {(-1)}^{j-1} \Delta^{j}{ \psi(c x) } ,
#'     \quad x > 0 .
#' }
#'
#' @inheritParams levyDensity
#' @param x a nonnegativ numeric vector at which the iterated difference of
#'   the Bernstein function is evaluated.
#' @param difference_order a nonnegative integer with the order of the
#'   alternating iterated forward differences taken on the Bernstein function.
#' @param cscale a positive numeric scalar with the composite scaling factor.
#' @param n,k nonnegative numbers for the binomial factor.
#' @param ... pass-through parameter.
#'
#' @importFrom methods setGeneric
#' @family Bernstein function generics
#' @export
setGeneric(
  "valueOf",
  function(object, x, difference_order = 0L, n = 1L, k = 0L, cscale = 1, ...) { # nolint
    standardGeneric("valueOf")
  }
)
