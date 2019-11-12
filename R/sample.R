#' Sampling from the exogenous shock model
#'
#' Draws `n` independent samples from a `d` variate Marshall-Olkin distribution
#' with shock rates `intensities` using the *exogenous shock model algorithm*.
#' The shock rates must be stored in a vector of length \eqn{2^d-1}. For more information
#' on this algorithm, see J.-F. Mai, M. Scherer, "Simulating Copulas", World Scientific (2017).
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



#' Sampling from the modified Arnold model for exchangeable MO
#'
#' Draws `n` independent samples from a `d` variate exchangeable Marshall-Olkin
#' distribution with shock rates `intensities` using the *exogenous shock model algorithm*.
#' The shock rates must be stored in a vector of length \eqn{2^d-1}. For more information
#' on this algorithm, see J.-F. Mai, M. Scherer, "Simulating Copulas", World Scientific (2017).
#'
#' @param n number of samples
#' @param d dimension
#' @param ex_intensities shock model intensity rates (length \eqn{d})
#'
#' @return an \eqn{n \times d}{n x d} matrix with rows corresponding to the
#' independent samples of size \eqn{d}.
#'
#' @examples
#' # sample 10 times from a bivariate exchangeable Marshall-Olkin distribution
#' # with parameters with c(1, 1) using the generalised Arnold model for the
#' # exchangeable subclass
#' rmo_ex_arnold(10, 2, c(1, 1))
#'
#' @export
rmo_ex_arnold <- function(n, d, ex_intensities) {
  out <- matrix(0, nrow=n, ncol=d)

  for (i in 1:n) {
    value <- rmo_ex_arnold_sorted(d, ex_intensities)
    perm <- sample.int(d, d, replace = FALSE)
    out[i, ] <- value[perm]
  }

  out
}

#' @importFrom stats rexp
#' @keywords internal
#' @noRd
rmo_ex_arnold_sorted <- function(d, ex_intensities) {
  transition_probabilities <- vapply(1:d, function(x) choose(d, x), FUN.VALUE = 0.5) *
    ex_intensities # intermediate result
  total_intensity <- sum(transition_probabilities)
  transition_probabilities <- transition_probabilities / total_intensity

  epsilon <- rexp(1, total_intensity)
  num_affected <- sample.int(d, 1, replace = FALSE, prob = transition_probabilities)

  if (d == num_affected) {
    return(rep(epsilon, d))
  }

  for (i in 1:num_affected) { ## if possible, replace with binomial expression
    ex_intensities <- ex_intensities[1:(d-i)] + ex_intensities[2:(d-i+1)]
  }

  epsilon + c(rep(0, num_affected),
              rmo_ex_arnold_sorted(d-num_affected, ex_intensities))
}
