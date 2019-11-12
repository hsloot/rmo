#' Sampling from the exogenous shock model
#'
#' Draws `n` independent samples from a `d` variate Marshall-Olkin distribution
#' with shock rates `intensities` using the *exogenous shock model algorithm*.
#' The shock rates must be stored in a vector of length \eqn{2^d-1}. For more information
#' on this algorithm, see J.-F. Mai, M. SCherer, "Simulating Copulas", World Scientific (2017).
#'
#' @param n number of samples
#' @param d dimension
#' @param intensities shock model intensity rates
#'
#' @return an \eqn{n \times d}{n x d} matrix with rows corresponding to the independent
#' samples of size \eqn{d}.
#'
#' @examples
#' # sample 10 times from a bivariate Marshall-Olkin distribution with parameters
#' # with c(1, 1, 1) using the exogenous shock model algorithm
#' rmo_esm(10, 2, c(1, 1, 1))
#'
#' @export
#' @importFrom stats rexp
#' @include sets.R
rmo_esm <- function(n, d, intensities) {
  out <- matrix(nrow=n, ncol=d)
  for (k in 1:n) {
    value <- rep(Inf, d)
    for (j in 1:(2^d - 1)) {
      E <- rexp(1, intensities[[j]]) # nolint
      for (i in 1:d) {
        if (is_within(i, j))
          value[i] <- min(c(value[[i]], E))
      }
    }
    out[k, ] <- value
  }

  out
}


#' Sampling from the Arnold model
#'
#' Draws `n` independent samples from a `d` variate Marshall-Olkin distribution
#' with shock rates `intensities` using the *Arnold model algorithm*. The shock
#' rates must be stored in a vactor of length \eqn{2^d-1}. For more information
#' on this algorithm, see J.-F. Mai, M. SCherer, "Simulating Copulas", World Scientific (2017).
#'
#' @param n number of samples
#' @param d dimension
#' @param intensities shock model intensity rates
#'
#' @return an \eqn{n \times d}{n x d} matrix with rows corresponding to the independent
#' samples of size \eqn{d}.
#'
#' @examples
#' # sample 10 times from a bivariate Marshall-Olkin distribution with parameters
#' # with c(1, 1, 1) using the Arnold model algorithm
#' rmo_arnold(10, 2, c(1, 1, 1))
#' # sample 10 times from a bivariate Marshall-Olkin distribution with parameters
#' # with c(0.5, 0.4, 0.6) using the Arnold model algorithm
#' rmo_arnold(10, 2, c(0.5, 0.4, 0.6))
#'
#' @export
#' @importFrom stats rexp
#' @include sets.R
rmo_arnold <- function(n, d, intensities) {
  total_intensity <- sum(intensities)
  transition_probs <- intensities / total_intensity
  out <- matrix(nrow=n, ncol=d)

  for (k in 1:n) {
    destroyed <- logical(d)
    value <- numeric(d)

    while (!all(destroyed)) {
      W <- rexp(1, total_intensity) # nolint
      Y <- sample.int(n = 2^d-1, size=1, replace=FALSE, prob = transition_probs) # nolint

      for (i in 1:d) {
        if (!destroyed[[i]]) {
          if (is_within(i, Y)) {
            destroyed[[i]] <- TRUE
          }
          value[[i]] <- value[[i]] + W
        }
      }
    }
    out[k, ] <- value
  }

  out
}
