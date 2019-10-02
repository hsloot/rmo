#' Sampling from the exogenous shock model
#'
#' Draws `n` independent samples from a `d` variate Marshall-Olkin distribution
#' with shock rates `intensities` using the *exogenous shock model algorithm*.
#' The shock rates must be stored in a vector of length \eqn{2^d-1}.
#'
#' @param n number of samples
#' @param d dimension
#' @param intensities shock model intensity rates
#'
#' @return an \eqn{n \times d} matrix with rows corresponding to the independent
#' samples of size d.
#'
#' @examples
#' rmo_esm(10, 2, c(1, 1, 1))
#'
#' @export
#' @importFrom stats rexp
#' @include sets.R
rmo_esm <- function(n, d, intensities) {
  out <- matrix(nrow=n, ncol=d)
  for (k in 1:n) {
    value <- rep(Inf, d)
    for (j in 1:(2^d-1)) {
      E <- rexp(1, intensities[[j]])
      for (i in 1:d) {
        if (is_within(i, j))
          value[i] <- min(c(value[[i]], E))
      }
    }
    out[k, ] <- value
  }

  out
}
