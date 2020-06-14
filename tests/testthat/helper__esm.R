#' Bivariate implementation
#'
#' @rdname rmo_esm
#' @keywords internal
#' @noRd
test__rmo_esm_bivariate <- function(n, d, intensities) { # nolint
  out <- matrix(0, nrow = n, ncol = 2)
    for (i in 1:n) {
      ## individual shock for component 1
      shock_for_1 <- rexp_if_rate_zero_then_infinity(1, intensities[[1]])
      ## individual shock for component 2
      shock_for_2 <- rexp_if_rate_zero_then_infinity(1, intensities[[2]])
      ## global shock for both compoenents
      shock_for_1_and_2 <- rexp_if_rate_zero_then_infinity(1, intensities[[3]])

      out[i, ] <- pmin(c(shock_for_1, shock_for_2), shock_for_1_and_2)
    }

  out
}


#' Original implementation in `R`
#'
#' @rdname rmo_esm
#' @keywords internal
#' @noRd
test__rmo_esm <- function(n, d, intensities) { # nolint
  out <- matrix(NA, nrow=n, ncol=d)
  for (k in 1:n) {
    ## initialise values for all components to Inf such that we can
    ## overwrite them at the first time a shock concerns this component
    value <- rep(Inf, d)
    ## iterate over all shocks
    for (j in 1:(2^d - 1)) {
      ## sample shock time
      shock_time <- rexp_if_rate_zero_then_infinity(1, intensities[[j]])
      ## iterate over all components, check if current shock concerns it,
      ## and update value if that is the case.
      for (i in 1:d) {
        if (test__is_within(i, j))
          value[i] <- min(c(value[[i]], shock_time))
      }
    }
    out[k, ] <- value
  }

  out
}
