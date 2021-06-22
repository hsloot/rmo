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
#'   It can be used to generate (unscaled) exchangeable intensities with
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
#' Multiple sampling algorithms are provided. The semantic naming scheme `r*mo_*`
#' is used, e.g. [rexmo_markovian()] has *(scaled) exchangeable intensities*
#' `ex_intensities` as input and uses the Markovian model for the default
#' counting process.
#' The following algorithms are implemented:
#' - *Exogenous shock model* for arbitrary Marshall-Olkin distributions with
#'   [rmo_esm()]. Not adviced for `d` much larger than 2.
#' - *Arnold model* for arbitrary Marshall-Olkin distributions with [rmo_arnold()].
#' - *Exchangeable Markovian model* for exchangeable Marshall-Olkin distributions
#'   with [rexmo_markovian()].
#' - *Lévy-frailty model for compound Poisson subordinators* for the corresponding
#'   subclass of extendible Marshall-Olkin distributions with [rextmo_lfm()].
#' - *Exogenous shock model* for the armageddon ESM subclass with [rarmextmo_esm()].
#'
#' @docType package
#' @name rmo
NULL
