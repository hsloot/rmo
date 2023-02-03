## usethis namespace: start
#' @useDynLib rmo, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom Rdpack reprompt
## usethis namespace: end
NULL

#' rmo: A package for the Marshall–Olkin distribution.
#'
#' The package contains fast implementations of sampling algorithms related to the Marshall–Olkin
#' distribution. It also contains an S4-class system for Bernstein functions which can be used to
#' create a large variety of Marshall–Olkin parameterisations.
#'
#' @section Bernstein functions:
#' A *Bernstein function* is a non-negative function with non-negative alternating iterative
#' differences. These functions can be used to generated parameterisations the extendible
#' Marshall–Olkin distributions.
#' - An object that derives from [BernsteinFunction-class] can be used to generate the
#'   Marshall–Olkin shock arrival intensities with [intensities()]. It can be used to generate
#'   (scaled) exchangeable shock-size arrival intensities with [exIntensities()].
#' - Many families of Bernstein functions are available, e.g. [ParetoBernsteinFunction-class],
#'   [ExponentialBernsteinFunction-class], and [AlphaStableBernsteinFunction-class].
#' - Bernstein functions can be recombined by scaling, by summation or by composition, which can be
#'   used to create new Bernstein functions with [ScaledBernsteinFunction-class] and
#'   [SumOfBernsteinFunctions-class].
#'
#'
#' @section Sampling:
#' Multiple sampling algorithms are provided. The semantic naming scheme `r*mo` is used, e.g.,
#' - [rmo()] allows the simulation from Marshall–Olkin distributions. It has *shock arrival
#'   intensities* as input and uses the *Arnold model* or *exogenous shock model* for sampling; the
#'   former can be used up until dimension \eqn{30}, but the latter should only be used in very
#'   small dimensions.
#' - [rexmo()] allows the simulation from exchangeable Marshall–Olkin distributions. It has
#'   *exchangeable shock-size arrival intensities* as input and uses the *Markovian death-set
#'   model*, the *Arnold model*, or the *exogenous shock model* for sampling; for the latter two
#'   models, the corresponding *shock arrival intensities* are calculated and passed down to
#'   [rmo()].
#' - [rextmo()] allows the simulation from extendible Marshall–Olkin distributions. It has a
#'   *Bernstein function* as input and calculates the corresponding *exchangeable shock-size arrival
#'   intensities* and passes them down to [rexmo()].
#' - [rpextmo()] allows the simulation from parametric families of extendible Marshall–Olkin
#'   distributions. It has a *killing-rate*, a *drift*, a *scaling factor*, a *parameter vector*,
#'   and a *family name* as input. All previous models can be used and the corresponding inputs are
#'   calculated from the Bernstein function. For special families, which correspond to Lévy
#'   exponents of compound Poisson subordinators, the *Lévy frailty model* can be used for
#'   simulations.
#'
#' @docType package
#' @name rmo-package
#' @aliases rmo-package NULL
#'
#' @keywords internal
"_PACKAGE"
