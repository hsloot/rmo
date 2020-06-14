#' @keywords internal
#' @noRd
bf2ex_intensities <- function(d, bf) {
  sapply(1:d, function(i) valueOf(bf, d-i, i))
}

#' @keywords internal
#' @noRd
ex_intensities2intensities <- function(ex_intensities) {
  d <- length(ex_intensities)
  intensities <- numeric(2^d-1)

  for (j in seq_along(intensities)) {
    count <- 0
    for (i in 1:d) {
      count <- count + Rcpp__is_within(i, j)
    }
    intensities[j] <- ex_intensities[count]
  }

  intensities
}


#' Convenience functions to MO parameters
#'
#' The following functions are convenience wrappers to create `intensities`
#' parameters for the MO distribution and `ex_intensities` parameters for the
#' exMO distribution.
#'
#' @param d Dimension of the distribution
#'
#' @details For more information, please read the corresponding  documentation
#' for the respective Bernstein function.
#'
#'
#' @include bernstein.R
#' @seealso [valueOf()]
#'
#' @name parameter
#' @md
NULL

#' @param constant Rate for global shock, passed to `constant` parameter for [ConstantBernsteinFunction-class]
#'
#' @seealso [ConstantBernsteinFunction-class]
#'
#' @examples
#' ex_intensities_constant(10L, constant=1)
#'
#' @export
#' @rdname parameter
ex_intensities_constant <- function(d, constant) {
  bf <- ConstantBernsteinFunction(constant=constant)
  bf2ex_intensities(d, bf)
}

#' @examples
#' intensities_constant(10L, constant=1)
#'
#' @export
#' @rdname parameter
intensities_constant <- function(d, constant) {
  ex_intensities2intensities(
    ex_intensities_constant(d, constant))
}


#' @param scale Rates for individual shocks, passed to `scale` parameter for [LinearBernsteinFunction-class]
#'
#' @seealso [LinearBernsteinFunction-class]
#'
#' @examples
#' ex_intensities_linear(10L, scale=1)
#'
#' @export
#' @rdname parameter
ex_intensities_linear <- function(d, scale) {
  bf <- LinearBernsteinFunction(scale=scale)
  bf2ex_intensities(d, bf)
}

#' @examples
#' intensities_linear(10L, scale=1)
#'
#' @export
#' @rdname parameter
intensities_linear <- function(d, scale) {
  ex_intensities2intensities(ex_intensities_linear(d, scale))
}


#' @param alpha Rate for individual shocks, passed to `scale` parameter for [LinearBernsteinFunction-class]
#' @param beta Rate for global shock, passed to `constant` parameter for [ConstantBernsteinFunction-class]
#'
#' @examples
#' ex_intensities_cuadras_auge(10L, alpha=1, beta=0.5)
#'
#' @export
#' @rdname parameter
ex_intensities_cuadras_auge <- function(d, alpha, beta) {
  ex_intensities_linear(d, scale=alpha) +
    ex_intensities_constant(d, constant=beta)
}

#' @examples
#' intensities_cuadras_auge(10L, alpha=1, beta=0.5)
#'
#' @export
#' @rdname parameter
intensities_cuadras_auge <- function(d, alpha, beta) {
  ex_intensities2intensities(ex_intensities_cuadras_auge(d,
    alpha=alpha, beta=beta))
}


#' @param lambda Rate for Poisson process, passed to `lambda` parameter [PoissonBernsteinFunction-class]
#' @param eta Jump size for Poisson process, passed to `eta` parameter of [PoissonBernsteinFunction-class]
#'
#' @seealso [PoissonBernsteinFunction-class]
#'
#' @examples
#' ex_intensities_poisson(10L, lambda = 0.5, eta = 0.2)
#'
#' @export
#' @rdname parameter
ex_intensities_poisson <- function(d, lambda, eta) {
  bf <- PoissonBernsteinFunction(lambda=lambda, eta=eta)
  bf2ex_intensities(d, bf)
}

#' @examples
#' intensities_poisson(10L, lambda = 0.5, eta = 0.2)
#'
#' @export
#' @rdname parameter
intensities_poisson <- function(d, lambda, eta) {
  ex_intensities2intensities(ex_intensities_poisson(d, lambda, eta))
}


#' @param alpha Alpha parameter for alpha-stable subordinator, passed to `alpha` parameter of [AlphaStableBernsteinFunction-class]
#'
#' @seealso [AlphaStableBernsteinFunction-class]
#'
#' @examples
#' ex_intensities_alpha_stable(10L, alpha=0.4)
#'
#' @export
#' @rdname parameter
ex_intensities_alpha_stable <- function(d, alpha) {
  bf <- AlphaStableBernsteinFunction(alpha=alpha)
  bf2ex_intensities(d, bf)
}

#' @examples
#' intensities_alpha_stable(10L, alpha=0.4)
#'
#' @export
#' @rdname parameter
intensities_alpha_stable <- function(d, alpha) {
  ex_intensities2intensities(ex_intensities_alpha_stable(d, alpha))
}


#' @param a Shape parameter for Gamma subordinator, passed to `a` parameter of [GammaBernsteinFunction-class]
#'
#' @seealso [GammaBernsteinFunction-class]
#'
#' @examples
#' ex_intensities_gamma(10L, a=0.3)
#'
#' @export
#' @rdname parameter
ex_intensities_gamma <- function(d, a) {
  bf <- GammaBernsteinFunction(a=a)
  bf2ex_intensities(d, bf)
}

#' @examples
#' intensities_gamma(10L, a=0.3)
#'
#' @export
#' @rdname parameter
intensities_gamma <- function(d, a) {
  ex_intensities2intensities(ex_intensities_gamma(d, a))
}
