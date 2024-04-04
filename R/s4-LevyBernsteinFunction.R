#' Virtual superclass for Bernstein functions with nonzero Lévy density
#'
#' A virtual superclass for all Bernstein functions which can represented
#' by a Lévy density (no drift or killing rate). That means that there exists
#' a Lévy measure \eqn{\nu} such that
#' \deqn{
#'   \psi(x) = \int_0^\infty (1 - e^{-ux}) \nu(du) , x > 0 .
#' }
#'
#' @seealso [BernsteinFunction-class], [valueOf()]
#'
#' @include s4-BernsteinFunction.R
#' @family Bernstein function classes
#' @family Virtual Bernstein function classes
#' @family Levy Bernstein function classes
#' @export
setClass("LevyBernsteinFunction",
  contains = c("BernsteinFunction", "VIRTUAL")
)

#' @include s4-defaultMethod.R
#' @keywords internal
setMethod(
  "defaultMethod", "LevyBernsteinFunction",
  function(object) {
    "levy"
  }
)

#' @rdname hidden_aliases
#'
#' @inheritParams valueOf
#' @param method Method to calculate the result; use `method = "levy"` for
#'   using the Lévy representation and `method = "stieltjes"` for using the
#'   Stieltjes representation.
#' @param tolerance (Relative) tolerance, passed down to [stats::integrate()]
#'
#' @details
#' ### Evaluation of Bernstein functions with Lévy densities
#'
#' For *continuous Lévy densities*, the values of the Bernstein function are
#' calculated with [stats::integrate()] by using the representation
#' \deqn{
#'   \psi(x)
#'     = \int_{0}^{\infty} (1 - \operatorname{e}^{-ux}) \nu(du), \quad x > 0 ,
#' }
#' and the values of the iterated differences are calculated by using the
#' representation
#' \deqn{
#'   (-1)^{j-1} \Delta^{j} \psi(x)
#'     = \int_{0}^{\infty}
#'       \operatorname{e}^{-ux} (1 - \operatorname{e}^{-u})^j \nu(du) ,
#'     \quad x > 0 .
#' }
#'
#' For *discrete Lévy densities* \eqn{\nu(du) = \sum_{i} y_i \delta_{u_i}(du)},
#' the values of the Bernstein function are calculated by using the
#' representation
#' \deqn{
#'   \psi(x)
#'     = \sum_{i} (1 - \operatorname{e}^{-u_i x}) y_i, \quad x > 0 ,
#' }
#' and the values of the iterated differences are calculated by using the
#' representation
#' \deqn{
#'   (-1)^{j-1} \Delta^{j} \psi(x)
#'     = \sum_{i} \operatorname{e}^{-u_i x} (1 - \operatorname{e}^{-u_i})^j y_i,
#'     \quad x > 0 .
#' }
#'
#' @include s4-valueOf.R s4-valueOf0.R RcppExports.R
#' @importFrom checkmate qassert
#' @importFrom stats integrate
#' @export
setMethod(
  "valueOf", "LevyBernsteinFunction",
  function(object, x, difference_order, n = 1L, k = 0L, cscale = 1, ...,
           method = c("default", "levy"),
           tolerance = .Machine$double.eps^0.5) {
    method <- match.arg(method)

    if (isTRUE("default" == method)) {
      if (isTRUE(0L == difference_order)) {
        qassert(cscale, "N1(0,)")
        qassert(n, "X1(0,)")
        qassert(k, "N1[0,)")
        out <- multiply_binomial_coefficient(
          valueOf0(object, x * cscale), n, k
        )
      } else if (isTRUE(1L == difference_order)) {
        out <- multiply_binomial_coefficient(
          valueOf0(object, (x + 1) * cscale), n, k
        ) -
          multiply_binomial_coefficient(
            valueOf0(object, x * cscale), n, k
          )
      } else {
        out <- valueOf(object, x, difference_order, n, k, cscale, ...,
          method = defaultMethod(object), tolerance = tolerance
        )
      }
    } else {
      qassert(x, "N+[0,)")
      qassert(difference_order, "X1[0,)")
      qassert(cscale, "N1(0,)")
      qassert(n, "X1(0,)")
      qassert(k, "N1[0,)")
      levy_density <- levyDensity(object)
      if (isTRUE(0L == difference_order)) {
        fct <- function(u, .x) {
          (1 - exp(-u * cscale * .x))
        }
      } else {
        fct <- function(u, .x) {
          exp(-u * cscale * .x) *
            (1 - exp(-u * cscale))^difference_order
        }
      }

      if (isTRUE("continuous" == attr(levy_density, "type"))) {
        integrand_fn <- function(u, .x) {
          multiply_binomial_coefficient(
            fct(u, .x) * levy_density(u), n, k
          )
        }
        out <- sapply(
          x,
          function(.x) {
            res <- integrate(integrand_fn,
              .x = .x,
              lower = attr(levy_density, "lower"),
              upper = attr(levy_density, "upper"),
              rel.tol = tolerance, stop.on.error = FALSE,
              ...
            )
            if (!isTRUE("OK" == res$message) &&
                  abs(.x) < 50 * .Machine$double.eps) {
              res <- integrate(integrand_fn,
                .x = 50 * .Machine$double.eps,
                lower = attr(levy_density, "lower"),
                upper = attr(levy_density, "upper"),
                rel.tol = tolerance, stop.on.error = FALSE,
                ...
              )
            }
            if (!isTRUE("OK" == res$message)) {
              stop(
                sprintf(
                  "Numerical integration failed with error: %s", # nolint
                  res$message
                )
              )
            }

            res$value
          }
        )
      } else {
        out <- multiply_binomial_coefficient(
          as.vector(
            levy_density$y %*% outer(levy_density$x, x, fct)
          ),
          n, k
        )
      }
    }

    out
  }
)
