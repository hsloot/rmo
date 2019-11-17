## #### Assertation error messages #####
##
# nolint start
ERR_MARGINRATE_NOT_POS = "%s does not have positive marginal rates"
# nolint end


## #### Miscellaneous custom assertations ####
##


## #### Assertations for MO params ####
##

#' Assertion for the  `intensities` parameter
#'
#' Assert if `intensities` is a valid Marshall-Olkin  parameter.
#' For this, the provided numeric vector must non-negative
#' and fulfil the property
#' \deqn{
#'   \sum_{I \ni i} \lambda_i > 0 \forall i \in \{ 1, \ldots, d\} .
#' }
#'
#' @param intensities A numeric vector intended to be the shock rates of
#'   a Marshall-Olkin distribution.
#'
#' @return TRUE/FALSE
#'
#' @importFrom assertthat assert_that on_failure<-
#' @include sets.R
#' @keywords internal
#' @noRd
is_mo_parameter <- function(intensities) {
  assert_that(is.numeric(intensities), length(intensities) >= 1, all(intensities >= 0))
  assert_that(log2(length(intensities)+1) %% 1 == 0)
  d <- log2(length(intensities)+1)
  marginal_intensities <- numeric(d)
  for (i in 1:d) {
    for (j in 1:(2^d-1)) {
      if (is_within(i, j)) {
        marginal_intensities[i] <- marginal_intensities[[i]] + intensities[[j]]
      }
    }
  }

  all(marginal_intensities > 0)
}
on_failure(is_mo_parameter) <- function(call, env) {
  sprintf(ERR_MARGINRATE_NOT_POS, deparse(call$intensities))
}


## #### Assertations for exMO params ####
##

#' Assertion for the `ex_intensities` parameter
#'
#' Assert if `ex_intensities` is a valid exchangeable Marshall-Olkin parameter.
#' For this, the numeric vector must be non-negative and fulfil the property
#' \deqn{
#'   \sum_{j=0}^{d-1} \choose{d-1}{j} \lambda_{j+1} > 0 .
#' }
#'
#' @param ex_intensity A numeric vector intended to be the shock rates of
#'   a Marshall-Olkin distribution.
#'
#' @return TRUE/FALSE
#'
#' @importFrom assertthat assert_that
#' @keywords internal
#' @noRd
is_exmo_parameter <- function(ex_intensities) {
  assert_that(is.numeric(ex_intensities), length(ex_intensities) >= 1, all(ex_intensities >= 0))
  d <- length(ex_intensities)
  marginal_intensity <- sum(vapply(0:(d-1), function(y) choose(d-1, y) * ex_intensities[[y+1]], FUN.VALUE=0.5)) # nolint

  marginal_intensity > 0
}
on_failure(is_exmo_parameter) <- function(call, env) {
  sprintf(ERR_MARGINRATE_NOT_POS, deparse(call$ex_intensities))
}
