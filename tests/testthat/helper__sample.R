## File contains test functions for various simulation Marshall--Olkin
## simulation algorithms.
##
## Naming convention for the functions: test__rmo_<name>_<specialisation>_R
##
## #### Setup ####
##
if (!"assertthat" %in% .packages()) {
  library("assertthat", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
}
## Use pre R 3.6.x sample RNG since the new one is not yet
## implemented in Rcpp.
suppressWarnings(RNGkind(sample.kind="Rounding"))

#' A wrapper for `rexp`
#'
#' Wraps an `ifelse`-clause arround `rexp` with special treatment for the case
#' `rate=0`.
#'
#' @inheritParams stats::rexp
#'
#' @importFrom stats rexp
#'
#' @keywords internal
#' @noRd
rexp_if_rate_zero_then_infinity <- function(n, rate) { # nolint
  if (0 == rate) {
    return(rep(Inf, n))
  }

  rexp(n, rate)
}
