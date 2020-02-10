#' Bivariate implementation of the exogenous shock model
#'
#' @rdname rmo_esm
#' @keywords internal
#' @noRd
test__rmo_esm_bivariate_R <- function(n, d, intensities) { # nolint
  out <- matrix(0, nrow = n, ncol = 2)
    for (i in 1:n) {
      shock_for_1 <- rexp_if_rate_zero_then_infinity(1, intensities[[1]])
      shock_for_2 <- rexp_if_rate_zero_then_infinity(1, intensities[[2]])
      shock_for_1_and_2 <- rexp_if_rate_zero_then_infinity(1, intensities[[3]])
      out[i, ] <- pmin(c(shock_for_1, shock_for_2), shock_for_1_and_2)
    }

  out
}


#' Original implementation of the exogenous shock model in `R`
#'
#' @rdname rmo_esm
#' @keywords internal
#' @noRd
test__rmo_esm_R <- function(n, d, intensities) { # nolint
  out <- matrix(NA, nrow=n, ncol=d)
  for (k in 1:n) {
    value <- rep(Inf, d)
    for (j in 1:(2^d - 1)) {
      shock_time <- rexp_if_rate_zero_then_infinity(1, intensities[[j]])
      for (i in 1:d) {
        if (test__is_within_R(i, j))
          value[i] <- min(c(value[[i]], shock_time))
      }
    }
    out[k, ] <- value
  }

  out
}
