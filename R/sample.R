## #### Sample from extMO distributions ####
##

#' Sample with LFM and CPP subordinator
#'
#' Draws `n` independent samples from a `d`-variate extendible Marshall-Olkin
#' distribution corresponding to a LFM with a compound Poisson subordinator with
#' parameters `rate`, `rate_killing`, `rate_drift`, `rjump_name`, and
#' `rjump_arg_list`.
#'
#' - `rate` is the *jump intensity* of the compound Poisson subordinator:
#' at each given point-in-time, the waiting time to the next jump is
#' exponentially distributed with rate `rate`.
#' - `rate_killing` is the *killing intensity* of the compound Poisson
#' subordinator: the probability that the compound Poisson subordinator jumps to
#' its graveyard-state \eqn{\infty} between \eqn{t} and \eqn{s} is \eqn{1 -
#' \exp{\{-{ratekilling} (s-t)\}}}{1-exp{-rate_killing * (s-t)}}.
#'
#' @param n number of samples
#' @param d dimension
#' @param rate rate of CPP subordinator
#' @param rate_killing killing rate of CPP subordinator
#' @param rate_drift drift rate of CPP subordinator
#' @param rjump_name name of jump sampling function for jumps of CPP
#' subordinator
#' @param rjump_arg_list list with named arguments for jump sampling function
#' for jumps of CPP subordinator
#'
#' @return `rmo_lfm_cpp` implements the Lévy-frailty model representation with a
#' compound Poisson subordinator and returns an \eqn{n \times d}{n x d} numeric
#' matrix with the rows corresponding to independent and identically
#' distributed samples of the corresponding `d`-variate extendible
#' Marshall-Olkin distribution.
#'
#' @section References: For more information on this algorithm, see J.-F. Mai,
#' M. Scherer, "Simulating Copulas", World Scientific (2017), pp. 140 psqq.
#'
#' @examples
#' rmo_lfm_cpp(10L, 2L, 0.5, 0.1, 0.2, "rposval", list("value"=1))
#' rmo_lfm_cpp(10L, 2L, 0.5, 0, 0, "rexp", list("rate"=2))
#' \dontrun{
#' rmo_lfm_cpp(10L, 2L, 0, 0, 1, "rposval", list("value"=1))  ## independence
#' rmo_lfm_cpp(10L, 2L, 0, 1, 0, "rposval", list("value"=1))  ## comonotone
#' }
#'
#' @family samplers
#'
#' @include assert.R
#' @importFrom assertthat assert_that is.count
#' @importFrom stats rexp
#'
#' @export
#' @name rmo_lfm_cpp
rmo_lfm_cpp <- function(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list = list()) {
  assert_that(is.count(n), is.count(d), is_nonnegative_number(rate),
    is_nonnegative_number(rate_killing), is_nonnegative_number(rate_drift),
    is_positive_number(rate + rate_killing + rate_drift),
    is_rjump_name(rjump_name), is_rjump_arg_list(rjump_name, rjump_arg_list))

  out <- matrix(NA, nrow=n, ncol=d)
  for (k in 1:n) {
    unit_exponentials <- rexp(d)
    cpp_subordinator <- sample_cpp(rate, rate_killing, rate_drift,
      rjump_name, rjump_arg_list, unit_exponentials)
    out[k, ] <- vapply(1:d, function(x) min(cpp_subordinator[cpp_subordinator[, 2] >= unit_exponentials[[x]], 1]), FUN.VALUE=0.5) # nolint
  }

  out
}



## #### Auxiliary samplers ####
##

#' A wrapper for `rexp`
#'
#' Wraps an `ifelse`-clause arround `rexp` with special treatment for the case
#' `rate=0`.
#'
#' @inheritParams stats::rexp
#'
#' @importFrom stats rexp
#'
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
#' @return A `n` elements numeric vector with `value` in each component
#'
#' @examples
#' rposval(10L)       ## rep(1, 10L)
#' rposval(10L, pi)   ## rep(pi, 10L)
#'
#' @family samplers
#'
#' @keywords internal
#' @noRd
rposval <- function(n, value=1) {
  rep(value, times=n)
}


#' @rdname rmo_lfm_cpp
#'
#' A sampling function for a (possibly killed) compound Poisson subordinator
#' with non-negative jump distribution.
#'
#' @inheritParams rmo_lfm_cpp
#' @param barrier_values a vector of barrier values from the LFM to properly
#' incorporate first exit times over these `barrier_values` if killing or drift
#' is present.
#'
#' @return A named `k x 2` array with names `c("t", "value")`, where `k` is
#' random and each row represents a time-value tupel for a jump in the compound
#' Poisson subordinator.
#'
#' @include assert.R
#' @importFrom stats rexp
#'
#' @keywords internal
#' @noRd
sample_cpp <- function(rate, rate_killing, rate_drift, rjump_name, rjump_arg_list, barrier_values) { # nolint
  if (rate_drift>0) {
    barrier_values <- sort(barrier_values)
  } else {
    barrier_values <- max(barrier_values)
  }

  times <- 0
  values <- 0
  for (i in seq_along(barrier_values)) {
    while (sum(values) < barrier_values[[i]]) {
      waiting_time <- rexp_if_rate_zero_then_infinity(1, rate)
      jump_value <- do.call(rjump_name, args=c("n"=1, rjump_arg_list))
      killing_time <- rexp_if_rate_zero_then_infinity(1, rate_killing)

      if (killing_time < Inf && killing_time <= waiting_time) {
        if (rate_drift>0 && (barrier_values[[i]] - sum(values))/rate_drift<=killing_time) {
          intermediate_time <- (barrier_values[i] - sum(values)) / rate_drift
          intermediate_value <- intermediate_time * rate_drift
          times <- c(times, intermediate_time)
          values <- c(values, intermediate_value)
          killing_time <- killing_time - intermediate_time
        }

        times <- c(times, killing_time)
        values <- c(values, Inf)
      } else {
        if (rate_drift>0 && (barrier_values[[i]] - sum(values))/rate_drift <= waiting_time) {
          intermediate_time <- (barrier_values[i] - sum(values)) / rate_drift
          intermediate_value <- intermediate_time * rate_drift
          times <- c(times, intermediate_time)
          values <- c(values, intermediate_value)
          waiting_time <- waiting_time - intermediate_time
        }

        times <- c(times, waiting_time)
        values <- c(values, waiting_time * rate_drift + jump_value)
      }
    }
  }

  cbind("t"=cumsum(times), "value"=cumsum(values))
}
