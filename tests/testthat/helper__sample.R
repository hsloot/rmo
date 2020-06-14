#' @keywords internal
#' @noRd
rexp_if_rate_zero_then_infinity <- function(n, rate) { # nolint
  if (0 == rate) {
    return(rep(Inf, n))
  } else if (Inf == rate) {
    return(rep(0, n))
  }

  rexp(n, rate)
}
