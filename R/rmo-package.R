## usethis namespace: start
#' @useDynLib rmo, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom Rdpack reprompt
## usethis namespace: end
NULL

#' rmo: A package for simulating Marshall–Olkin distributions
#'
#' The rmo-package provides efficient sampling algorithms for the Marshall–Olkin
#' distribution and a flexible S4-class system for creating diverse
#' parametrizations.
#'
#' @section Sampling:
#' Simulation algorithms are provided for various MO parametrizations. The
#' semantic naming scheme `r*mo` is used, e.g.,
#'
#' - [rpextmo()] allows to simulate from parametric families of extendible
#'   Marshall–Olkin distributions. The function takes a *killing-rate*, a
#'   *drift*, a *scaling factor*, a *parameter vector*, and a *family name* as
#'   input.
#' - [rextmo()] allows to simulate from extendible Marshall–Olkin
#'   distributions. It takes a *Bernstein function* as input.
#' - [rexmo()] allows to simulate from exchangeable Marshall–Olkin
#'   distributions. It takes a vector of *exchangeable shock-size arrival
#'   intensities* as input.
#' - [rmo()] allows to simulate from Marshall–Olkin distributions. It takes
#'   vector of *shock arrival intensities* as input and uses the *Arnold model*
#'   or *exogenous shock model* for sampling; the former can be used up until
#'   dimension \eqn{30}, but the latter should only be used in very small
#'   dimensions.
#'
#' The default simulation algorithm is the *Markovian death-counting model*.
#' Dependent on the parametrization, other algorithms can be used, e.g., the
#' *exogenous shock model*, the *Arnold model*, or the *Lévy-frailty model*.
#'
#' @section Bernstein functions:
#' A *Bernstein function* can be used to parametrize the *extendible
#' Marshall–Olkin distribution*.
#'
#' - Many families of Bernstein functions are available,
#'   e.g. [ParetoBernsteinFunction-class],
#'   [ExponentialBernsteinFunction-class], and
#'   [AlphaStableBernsteinFunction-class].
#' - Bernstein functions can be recombined by scaling, by summation or by
#'   composition, which can be used to create new Bernstein functions with
#'   [ScaledBernsteinFunction-class], [CompositeScaledBernsteinFunction-class]
#'   and [SumOfBernsteinFunctions-class].
#' - An object that derives from [BernsteinFunction-class] can be used to
#'   generate the Marshall–Olkin shock arrival intensities with
#'   [calcShockArrivalIntensities()]. It can be used to generate (scaled)
#'   exchangeable shock-size arrival intensities with
#'   [calcExShockSizeArrivalIntensities()].
#'
#' @docType package
#' @name rmo-package
#' @aliases rmo-package NULL
#'
#' @keywords internal
"_PACKAGE"
