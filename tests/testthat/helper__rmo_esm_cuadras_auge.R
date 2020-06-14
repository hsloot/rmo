#' Bivariate implementatino of the Cuadras-Augé ESM
#'
#' @keywords internal
#' @noRd
test__rmo_esm_cuadras_auge_bivariate <- function(n, d, alpha, beta) { # nolint
  out <- matrix(NA, nrow=n, ncol=2L)
  for (k in 1:n) {
    ## sample the global shock
    global_shock <- rexp_(1L, beta) # nolint
    ## sample the individual shocks
    individual_shock_1 <- rexp_(1L, alpha) # nolint
    individual_shock_2 <- rexp_(1L, alpha) # nolint
    out[k, ] <- pmin(
      c(individual_shock_1, individual_shock_2),
      rep(global_shock, 2L))
  }

  out
}


#' Original implementation of the Cuadras-Augé ESM in `R`
#'
#' @keywords internal
#' @noRd
test__rmo_esm_cuadras_auge <- function(n, d, alpha, beta) { # nolint
  out <- matrix(NA, nrow=n, ncol=d)
  for (k in 1:n) {
    ## sample the global shock
    global_shock <- rexp_(1L, beta) # nolint
    ## sample the individual shocks
    individual_shocks <- rexp_(d, alpha) # nolint
    out[k, ] <- pmin(individual_shocks, rep(global_shock, d))
  }

  out
}
