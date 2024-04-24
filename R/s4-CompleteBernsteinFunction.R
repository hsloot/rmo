#' Virtual superclass for complete Bernstein functions
#'
#' A virtual superclass for all Bernstein functions which can represented
#' by a Stieltjes density (no drift or killing rate). That means that there
#' exists a Stieltjes measure \eqn{\sigma} such that
#' \deqn{
#'   \psi(x) = \int_0^\infty \frac{x}{x + u} \sigma(du) , x > 0 .
#' }
#'
#' @details
#' ### Evaluation of Complete Bernstein functions
#'
#' For *continuous Stieltjes densities*, the values of the Bernstein function
#' are calculated with [stats::integrate()] by using the representation
#' \deqn{
#'   \psi(x)
#'     = \int_{0}^{\infty} x \mathrm{Beta}(1, x + u) \sigma(du), \quad x > 0 ,
#' }
#' and the values of the iterated differences are calculated by using the
#' representation
#' \deqn{
#'   (-1)^{j-1} \Delta^{j} \psi(x)
#'     = \int_{0}^{\infty} u \mathrm{Beta}(j+1, x + u) \sigma(du) ,
#'     \quad x > 0 .
#' }
#'
#' For *discrete Lévy densities*
#' \eqn{\sigma(du) = \sum_{i} y_i \delta_{u_i}(du)},
#' the values of the Bernstein function are calculated by using the
#' representation
#' \deqn{
#'   \psi(x)
#'     = \sum_{i} x \mathrm{Beta}(1, x + u_i) y_i, \quad x > 0 ,
#' }
#' and the values of the iterated differences are calculated by using the
#' representation
#' \deqn{
#'   (-1)^{j-1} \Delta^{j} \psi(x)
#'     = \sum_{i} u_i \mathrm{Beta}(j+1, x + u_i) y_i ,
#'     \quad x > 0 .
#' }
#'
#' @seealso [levyDensity()], [stieltjesDensity()], [valueOf()],
#'   [intensities()], [uexIntensities()], [calcExShockSizeArrivalIntensities()],
#'   [exQMatrix()], [rextmo()], [rpextmo()]
#'
#' @docType class
#' @name CompleteBernsteinFunction-class
#' @rdname CompleteBernsteinFunction-class
#' @include s4-BernsteinFunction.R s4-LevyBernsteinFunction.R
#' @family Bernstein function classes
#' @family Virtual Bernstein function classes
#' @family Levy Bernstein function classes
#' @family Stieltjes Bernstein function classes
#' @export
setClass("CompleteBernsteinFunction",
  contains = c("LevyBernsteinFunction", "VIRTUAL")
)

#' @rdname hidden_aliases
#'
#' @inheritParams valueOf
#' @param method Method to calculate the result; use `method = "levy"` for
#'   using the Lévy representation and `method = "stieltjes"` for using the
#'   Stieltjes representation.
#' @param tolerance (Relative) tolerance, passed down to [stats::integrate()].
#'
#' @include s4-valueOf0.R s4-valueOf.R RcppExports.R
#' @importFrom checkmate qassert
#' @importFrom stats integrate
#' @export
setMethod(
  "valueOf", "CompleteBernsteinFunction",
  function(object, x, difference_order, n = 1L, k = 0L, cscale = 1, ...,
           method = c("default", "stieltjes", "levy"),
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
    } else if (isTRUE("stieltjes" == method)) {
      qassert(x, "N+[0,)")
      qassert(difference_order, "X1[0,)")
      qassert(cscale, "N1(0,)")
      qassert(n, "X1(0,)")
      qassert(k, "N1[0,)")
      stieltjes_density <- stieltjesDensity(object)
      if (isTRUE(0L == difference_order)) {
        fct <- function(u, .x) {
          .x * beta(1, .x + u / cscale)
        }
      } else {
        fct <- function(u, .x) {
          (u / cscale) * beta(difference_order + 1L, .x + u / cscale)
        }
      }
      if (isTRUE("continuous" == attr(stieltjes_density, "type"))) {
        integrand_fn <- function(u, .x) {
          multiply_binomial_coefficient(
            fct(u, .x) * stieltjes_density(u), n, k
          )
        }
        out <- sapply(
          x,
          function(.x) {
            res <- integrate(integrand_fn,
              .x = .x,
              lower = attr(stieltjes_density, "lower"),
              upper = attr(stieltjes_density, "upper"),
              rel.tol = tolerance, stop.on.error = FALSE,
              ...
            )
            if (!isTRUE("OK" == res$message) &&
                  abs(.x) < 50 * .Machine$double.eps) {
              res <- integrate(integrand_fn,
                .x = 50 * .Machine$double.eps,
                lower = attr(stieltjes_density, "lower"),
                upper = attr(stieltjes_density, "upper"),
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
            stieltjes_density$y %*%
              outer(stieltjes_density$x, x, fct)
          ),
          n, k
        )
      }
    } else {
      out <- callNextMethod()
    }

    out
  }
)
