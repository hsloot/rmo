## usethis namespace: start
#' @useDynLib rmo, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom Rdpack reprompt
## usethis namespace: end
NULL

#' rmo: A package for the Marshall-Olkin distribution.
#'
#' The package contains fast implementations of sampling algorithms related to
#' the Marshall-Olkin distribution. It also contains an S4-class system for
#' Bernstein functions which can be used to create a large variety of
#' Marshall-Olkin parameterisations.
#'
#' @section Bernstein functions:
#' A *Bernstein function* is a non-negative function with non-negative
#' alternating iterative differences. These functions can be used to
#' generated parameterisations for the exchangeable Marshall-Olkin
#' distribution.
#' - An object that derives from [BernsteinFunction-class] can be used to
#'   generate the Marshall-Olkin intensities with [intensities()].
#'   It can be used to generate (scaled) exchangeable intensities with
#'   [exIntensities()].
#' - Many families of Bernstein functions are available, e.g.
#'   [ParetoBernsteinFunction-class], [ExponentialBernsteinFunction-class], and
#'   [AlphaStableBernsteinFunction-class].
#' - Bernstein functions can be recombined by scaling, by summation or by
#'   composition, which can be used to create new Bernstein functions with
#'   [ScaledBernsteinFunction-class] and [SumOfBernsteinFunctions-class].
#'
#'
#' @section Sampling:
#' Multiple sampling algorithms are provided. The semantic naming scheme `r*mo` is used, e.g.,
#' [rexmo()] has *(scaled) exchangeable shock-size arrival intensities* `ex_intensities` as input
#' and uses, as the default, the Markovian model for the death-counting process for sampling.
#' The following algorithms are implemented:
#' - The *exogenous shock model* and the *Arnold model* for the general Marshall-Olkin distribution
#'   are implemented in [rmo()]. The former should only be used in very small dimensions and the
#'   latter can be used up until \eqn{d \leq 30}.
#' - *Markovian death-counting model* for exchangeable Marshall-Olkin distributions with [rexmo()].
#' - *Lévy-frailty model for compound Poisson subordinators* for the corresponding
#'   subclass of extendible Marshall-Olkin distributions with [rextmo_lfm()].
#' - *Exogenous shock model* for the armageddon ESM subclass with [rarmextmo_esm()].
#'
#' @docType package
#' @name rmo-package
NULL
