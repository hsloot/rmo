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
  assert_that(is.count(n), is.count(d), is_mo_parameter(intensities),
    length(intensities) == 2^d-1)

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
  assert_that(is.count(n), is.count(d), is_mo_parameter(intensities),
    length(intensities) == 2^d-1)

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
  assert_that(is.count(n), is.count(d), is_exmo_parameter(ex_intensities),
    length(ex_intensities) == d)

  out <- matrix(0, nrow=n, ncol=d)

  generator_list <- list()
  for (i in 1:d) {
    transition_probabilities <- vapply(1:i, function(x) sum(vapply(0:(d-i), function(y) choose((d-i), y) * ex_intensities[[x + y]], FUN.VALUE=0.5)) , FUN.VALUE=0.5) * vapply(1:i, function(x) choose(i, x), FUN.VALUE = 0.5) # nolint
    total_intensity <- sum(transition_probabilities)
    transition_probabilities <- transition_probabilities / total_intensity
    generator_list[[i]] <- list(total_intensity = total_intensity,
                                transition_probabilities = transition_probabilities)
  }

  for (i in 1:n) {
    value <- rmo_ex_arnold_sorted(d, generator_list)
    perm <- sample.int(d, d, replace = FALSE)
    out[i, ] <- value[perm]
  }

  out
}

#' @importFrom stats rexp
#' @keywords internal
#' @noRd
rmo_ex_arnold_sorted <- function(d, generator_list) {
  total_intensity <- generator_list[[d]]$total_intensity
  transition_probabilities <- generator_list[[d]]$transition_probabilities

  epsilon <- rexp(1, total_intensity)
  num_affected <- sample.int(d, 1, replace = FALSE, prob = transition_probabilities)

  if (d == num_affected) {
    return(rep(epsilon, d))
  }

  epsilon + c(rep(0, num_affected),
              rmo_ex_arnold_sorted(d-num_affected, generator_list))
}
