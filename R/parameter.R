#' @keywords internal
#' @noRd
bf2intensities <- function(d, bf) { # nocov start
    intensities(bf, d)
} # nocov end

#' @keywords internal
#' @noRd
bf2ex_intensities <- function(d, bf) { # nocov start
    exIntensities(bf, d)
} # nocov end


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
#' @seealso [valueOf()]
#'
#' @name parameter
#' @md
NULL

#' @param constant Rate for global shock, passed to `constant` parameter for
#'   [ConstantBernsteinFunction-class]
#'
#' @seealso [ConstantBernsteinFunction-class]
#'
#' @examples
#' ex_intensities_constant(10L, constant = 1)
#'
#' @export
#' @rdname parameter
ex_intensities_constant <- function(d, constant) { # nocov start
    bf <- ConstantBernsteinFunction(constant = constant)
    bf2ex_intensities(d, bf)
} # nocov end

#' @examples
#' intensities_constant(10L, constant = 1)
#'
#' @export
#' @rdname parameter
intensities_constant <- function(d, constant) { # nocov start
    bf <- ConstantBernsteinFunction(constant = constant)
    bf2intensities(d, bf)
} # nocov end


#' @param scale Rates for individual shocks, passed to `scale` parameter for
#'   [LinearBernsteinFunction-class]
#'
#' @seealso [LinearBernsteinFunction-class]
#'
#' @examples
#' ex_intensities_linear(10L, scale = 1)
#'
#' @export
#' @rdname parameter
ex_intensities_linear <- function(d, scale) { # nocov start
    bf <- LinearBernsteinFunction(scale = scale)
    bf2ex_intensities(d, bf)
} # nocov end

#' @examples
#' intensities_linear(10L, scale = 1)
#'
#' @export
#' @rdname parameter
intensities_linear <- function(d, scale) { # nocov start
    bf <- LinearBernsteinFunction(scale = scale)
    bf2intensities(d, bf)
} # nocov end


#' @param alpha Rate for individual shocks, passed to `scale` parameter for
#'   [LinearBernsteinFunction-class]
#' @param beta Rate for global shock, passed to `constant` parameter for
#'   [ConstantBernsteinFunction-class]
#'
#' @examples
#' ex_intensities_armageddon(10L, alpha = 1, beta = 0.5)
#'
#' @export
#' @rdname parameter
ex_intensities_armageddon <- function(d, alpha, beta) { # nocov start
    ex_intensities_linear(d, scale = alpha) +
        ex_intensities_constant(d, constant = beta)
} # nocov end

#' @examples
#' intensities_armageddon(10L, alpha = 1, beta = 0.5)
#'
#' @export
#' @rdname parameter
intensities_armageddon <- function(d, alpha, beta) { # nocov start
    intensities_linear(d, scale = alpha) +
        intensities_constant(d, constant = beta)
} # nocov end


#' @param eta Jump size for Poisson process, passed to `eta` parameter of
#'   [PoissonBernsteinFunction-class]
#'
#' @seealso [PoissonBernsteinFunction-class]
#'
#' @examples
#' ex_intensities_poisson(10L, eta = 0.2)
#'
#' @export
#' @rdname parameter
ex_intensities_poisson <- function(d, eta) { # nocov start
    bf <- PoissonBernsteinFunction(eta = eta)
    bf2ex_intensities(d, bf)
} # nocov end

#' @examples
#' intensities_poisson(10L, eta = 0.2)
#'
#' @export
#' @rdname parameter
intensities_poisson <- function(d, eta) { # nocov start
    bf <- PoissonBernsteinFunction(eta = eta)
    bf2intensities(d, bf)
} # nocov end


#' @param alpha Alpha parameter for alpha-stable subordinator, passed to `alpha`
#'   parameter of [AlphaStableBernsteinFunction-class]
#'
#' @seealso [AlphaStableBernsteinFunction-class]
#'
#' @examples
#' ex_intensities_alpha_stable(10L, alpha = 0.4)
#'
#' @export
#' @rdname parameter
ex_intensities_alpha_stable <- function(d, alpha) { # nocov start
    bf <- AlphaStableBernsteinFunction(alpha = alpha)
    bf2ex_intensities(d, bf)
} # nocov end

#' @examples
#' intensities_alpha_stable(10L, alpha = 0.4)
#'
#' @export
#' @rdname parameter
intensities_alpha_stable <- function(d, alpha) { # nocov start
    bf <- AlphaStableBernsteinFunction(alpha = alpha)
    bf2intensities(d, bf)
} # nocov end


#' @param lambda lambda parameter for Exponential-jump CPP subordinator, passed
#'   to `lambda` parameter of [ExponentialBernsteinFunction-class]
#'
#' @seealso [ExponentialBernsteinFunction-class]
#'
#' @examples
#' ex_intensities_exponential(10L, lambda = 0.4)
#'
#' @export
#' @rdname parameter
ex_intensities_exponential <- function(d, lambda) { # nocov start
    bf <- ExponentialBernsteinFunction(lambda = lambda)
    bf2ex_intensities(d, bf)
} # nocov end

#' @examples
#' intensities_exponential(10L, lambda = 0.4)
#'
#' @export
#' @rdname parameter
intensities_exponential <- function(d, lambda) { # nocov start
    bf <- ExponentialBernsteinFunction(lambda = lambda)
    bf2intensities(d, bf)
} # nocov end


#' @param a Shape parameter for Gamma subordinator, passed to `a` parameter of
#'   [GammaBernsteinFunction-class]
#'
#' @seealso [GammaBernsteinFunction-class]
#'
#' @examples
#' ex_intensities_gamma(10L, a = 0.3)
#'
#' @export
#' @rdname parameter
ex_intensities_gamma <- function(d, a) { # nocov start
    bf <- GammaBernsteinFunction(a = a)
    bf2ex_intensities(d, bf)
} # nocov end

#' @examples
#' intensities_gamma(10L, a = 0.3)
#'
#' @export
#' @rdname parameter
intensities_gamma <- function(d, a) { # nocov start
    bf <- GammaBernsteinFunction(a = a)
    bf2intensities(d, bf)
} # nocov end


#' @param alpha Alpha parameter for Pareto CPP, passed to `alpha`
#'   parameter of [ParetoBernsteinFunction-class]
#' @param x0 Cutoff parameter for Pareto CPP, passed to `x0`
#'   parameter of [ParetoBernsteinFunction-class]
#'
#' @seealso [ParetoBernsteinFunction-class]
#'
#' @examples
#' ex_intensities_pareto(10L, alpha = 0.4, x0 = 5e-4)
#'
#' @export
#' @rdname parameter
ex_intensities_pareto <- function(d, alpha, x0) { # nocov start
    bf <- ParetoBernsteinFunction(alpha = alpha, x0 = x0)
    bf2ex_intensities(d, bf)
} # nocov end

#' @examples
#' intensities_pareto(10L, alpha = 0.4, x0 = 5e-4)
#'
#' @export
#' @rdname parameter
intensities_pareto <- function(d, alpha, x0) { # nocov start
    bf <- ParetoBernsteinFunction(alpha = alpha, x0 = x0)
    bf2intensities(d, bf)
} # nocov end


#' @param eta `eta` parameter for the inverse Gaussian BF, passed to `eta`
#'   parameter of [InverseGaussianBernsteinFunction-class]
#'
#' @seealso [InverseGaussianBernsteinFunction-class]
#'
#' @examples
#' ex_intensities_inverse_gaussian(10L, eta = 0.4)
#'
#' @export
#' @rdname parameter
ex_intensities_inverse_gaussian <- function(d, eta) { # nolint # nocov start
    bf <- InverseGaussianBernsteinFunction(eta = eta)
    bf2ex_intensities(d, bf)
} # nocov end

#' @examples
#' intensities_inverse_gaussian(10L, eta = 0.4)
#'
#' @export
#' @rdname parameter
intensities_inverse_gaussian <- function(d, eta) { # nocov start
    bf <- InverseGaussianBernsteinFunction(eta = eta)
    bf2intensities(d, bf)
} # nocov end
