#' Bivariate implementatino of the Cuadras-Augé ESM
#'
#' @keywords internal
#' @noRd
test__rmo_esm_cuadras_auge_bivariate_R <- function(n, d, alpha, beta) { # nolint
  out <- matrix(NA, nrow=n, ncol=2L)
  for (k in 1:n) { # use rexp_if_rate_zero_then_infinity from `R/sample.R`
    individual_shock_1 <- rexp_if_rate_zero_then_infinity(1L, alpha)
    individual_shock_2 <- rexp_if_rate_zero_then_infinity(1L, alpha)
    global_shock <- rexp_if_rate_zero_then_infinity(1L, beta)
    out[k, ] <- pmin(c(individual_shock_1, individual_shock_2), rep(global_shock, 2L))
  }

  out
}


#' Original implementation of the Cuadras-Augé ESM in `R`
#'
#' @keywords internal
#' @noRd
test__rmo_esm_cuadras_auge_R <- function(n, d, alpha, beta) { # nolint
  out <- matrix(NA, nrow=n, ncol=d)
  for (k in 1:n) { # use rexp_if_rate_zero_then_infinity from `R/sample.R`
    individual_shocks <- rexp_if_rate_zero_then_infinity(d, alpha)
    global_shock <- rexp_if_rate_zero_then_infinity(1L, beta)
    out[k, ] <- pmin(individual_shocks, rep(global_shock, d))
  }

  out
}
