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
#' @importFrom assertthat assert_that is.count
#' @include sets.R
rmo_esm <- function(n, d, intensities) {
  assert_that(is.count(n), is.count(d), is_mo_parameter(intensities),
    length(intensities) == 2^d-1)

  out <- matrix(nrow=n, ncol=d)
  for (k in 1:n) {
    value <- rep(Inf, d)
    for (j in 1:(2^d - 1)) {
      shock_time <- rexp_if_rate_zero_then_infinity(1, intensities[[j]])
      for (i in 1:d) {
        if (is_within(i, j))
          value[i] <- min(c(value[[i]], shock_time))
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
#' @importFrom assertthat assert_that is.count
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
      waiting_time <- rexp(1, total_intensity)
      affected <- sample.int(n=2^d-1, size=1, replace=FALSE, prob=transition_probs)

      for (i in 1:d) {
        if (!destroyed[[i]]) {
          if (is_within(i, affected)) {
            destroyed[[i]] <- TRUE
          }
          value[[i]] <- value[[i]] + waiting_time
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
#' @importFrom assertthat assert_that is.count
rmo_ex_arnold <- function(n, d, ex_intensities) {
  assert_that(is.count(n), is.count(d), is_exmo_parameter(ex_intensities),
    length(ex_intensities) == d)

  generator_list <- list()
  for (i in 1:d) {
    transition_probs <- vapply(1:i, function(x) sum(vapply(0:(d-i), function(y) choose((d-i), y) * ex_intensities[[x + y]], FUN.VALUE=0.5)) , FUN.VALUE=0.5) * vapply(1:i, function(x) choose(i, x), FUN.VALUE = 0.5) # nolint intermediate result
    total_intensity <- sum(transition_probs)
    transition_probs <- transition_probs / total_intensity
    generator_list[[i]] <- list("total_intensity" = total_intensity,
                                "transition_probs" = transition_probs)
  }

  out <- matrix(0, nrow=n, ncol=d)
  for (k in 1:n) {
    value <- rmo_ex_arnold_sorted(d, generator_list)
    perm <- sample.int(d, d, replace = FALSE)
    out[k, ] <- value[perm]
  }

  out
}

#' Sampling from a sorted version of an exchangeable exMO distribution
#'
#' Samples *one* random vector which has the distribution of an ascendingly
#' sorted sample of an exchangeable Marshall-Olkin distribution with the
#' exchangeable shock intensities `ex_intensities`.
#'
#' @param d dimension
#' @param generator_list `list` of length `d` with the i-th element containing
#'   named elements with the  `total_intensity` and `transition_probs` of the
#'   i-marginal exMO model.
#'
#' @return A `numeric` vector of length `d`.
#'
#' The function currently does *not* perform any argument checks and calls
#' itself recursively. For more information
#' on this algorithm, see
#' J.-F. Mai, M. Scherer, "Simulating Copulas", World Scientific (2017).
#'
#' @importFrom stats rexp
#' @keywords internal
#' @noRd
rmo_ex_arnold_sorted <- function(d, generator_list) {
  total_intensity <- generator_list[[d]]$total_intensity
  transition_probs <- generator_list[[d]]$transition_probs

  waiting_time <- rexp(1, total_intensity)
  num_affected <- sample.int(d, 1, replace=FALSE, prob=transition_probs)

  if (d == num_affected) {
    return(rep(waiting_time, d))
  }

  waiting_time + c(rep(0, num_affected),
              rmo_ex_arnold_sorted(d-num_affected, generator_list))
}

#' @importFrom stats rexp
#' @keywords internal
#' @noRd
rexp_if_rate_zero_then_infinity <- function(n, rate) { # nolint
  if (rate == 0) {
    return(rep(Inf, n))
  }

  rexp(n, rate)
}

#' A dummy sampling function for deterministic, positive values
#'
#' @param n number of samples
#' @param value value to sample
#'
#' @return A `n` elements numeric vector with value `value` in each component
#'
#' @examples
#' rposval(10L) ## rep(1, 10L)
#' rposval(10L, pi) ## rep(pi, 10L)
#'
#' @family samplers
#'
#' @include assert.R
#'
#' @importFrom assertthat assert_that is.count
#' @keywords internal
#' @noRd
rposval <- function(n, value=1) {
  assert_that(is.count(n), is_positive_number(value))

  rep(value, times=n)
}
