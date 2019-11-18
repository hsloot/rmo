#' rmo: A package for the Marshall-Olkin distribution.
#'
#' The package contains an implementation of the sampling algorithms related
#' to the Marshall-Olkin distribution.
#'
#' @section Sampling:
#' - Sample with the *exogenous shock model* from arbitrary Marshall-Olkin
#'   distributions with \code{\link{rmo_esm}}. Not adviced for `d` much larger
#'   than 2.
#' - Sample with the *Arnold model* from arbitrary Marshall-Olkin distributions
#'   with \code{\link{rmo_arnold}}.
#' - Sample with the *modified Arnold model* from exchangeable Marshall-Olkin
#'   distributions with \code{\link{rmo_ex_arnold}}.
#' - Sample with the *Lévy-frailty model for compound Poisson subordinators*
#'   from the corresponding subclass of extendible Marshall-Olkin distributions
#'   with \code{\link{rmo_lfm_cpp}}.
#' - Sample with the optimised version of the *exogenous shock model* for the
#'   Cuadras-Augé subclass with \code{\link{rmo_esm_cuadras_auge}}.
#'
#' @docType package
#' @name rmo
NULL
