#' Bivariate implementation of the Compound Poisson process LFM
#'
#' @rdname rmo_lfm_cpp
#' @keywords internal
#' @noRd
test__rmo_lfm_cpp_bivariate_R <- function(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list) { # nolint

  if (rjump_name == "rexp") {
    return(test__rmo_lfm_cpp_bivariate_rexp_R(n, rate, rate_killing,
      rate_drift, rjump_arg_list$rate))
  } else if (rjump_name == "rposval") {
    return(test__rmo_lfm_cpp_bivariate_rposval_R(n, rate, rate_killing,
      rate_drift, rjump_arg_list$value))
  }
}

#' @rdname rmo_lfm_cpp
#' @keywords internal
#' @noRd
test__rmo_lfm_cpp_bivariate_rexp_R <- function(n, rate, rate_killing, rate_drift, jump_rate) { # nolint

  out <- matrix(NA, nrow=n, ncol=2L)
  for (k in 1:n) {
    unit_exponentials <- rexp(2L, 1)
    if (rate_drift>0.) {
      barrier_values <- sort(unit_exponentials)
    } else {
      barrier_values <- max(unit_exponentials)
    }
    d <- length(barrier_values)

    killing_waiting_time <- rexp_if_rate_zero_then_infinity(1, rate_killing)

    times <- 0.
    values <- 0.
    for (i in 1:d) {
      while (last(values) < barrier_values[[i]]) {
        waiting_time <- rexp_if_rate_zero_then_infinity(1, rate)
        jump_value <- rexp_if_rate_zero_then_infinity(1, jump_rate)

        if (killing_waiting_time < Inf && killing_waiting_time <= waiting_time) {
          for (j in i:d) {
            if (rate_drift>0. &&
                (barrier_values[[j]] - last(values))/rate_drift<=killing_waiting_time) {
              intermediate_waiting_time <- (barrier_values[[j]] - last(values)) / rate_drift
              times <- c(times, last(times) + intermediate_waiting_time)
              values <- c(values, barrier_values[[j]])
              killing_waiting_time <- killing_waiting_time - intermediate_waiting_time
            }
          }

          times <- c(times, last(times) + killing_waiting_time)
          values <- c(values, Inf)
        } else {
          for (j in i:d) {
            if (rate_drift>0. && (barrier_values[[j]] - last(values))/rate_drift <= waiting_time) {
              intermediate_waiting_time <- (barrier_values[[j]] - last(values)) / rate_drift
              times <- c(times, last(times) + intermediate_waiting_time)
              values <- c(values, barrier_values[[j]])
              waiting_time <- waiting_time - intermediate_waiting_time
            }
          }

          if (rate>0) { ## waiting_time<Inf # nolint
            times <- c(times, last(times) + waiting_time)
            values <- c(values, last(values) + waiting_time * rate_drift + jump_value)
          }
        }
      }
    }

    cpp_subordinator <- cbind("t"=times, "values"=values)
    out[k, ] <- vapply(1:2L, function(x) min(cpp_subordinator[cpp_subordinator[, 2] >= unit_exponentials[[x]], 1]), FUN.VALUE=0.5) # nolint
  }

  out
}

#' @rdname rmo_lfm_cpp
#' @keywords internal
#' @noRd
test__rmo_lfm_cpp_bivariate_rposval_R <- function(n, rate, rate_killing, rate_drift, jump_value) { # nolint

  out <- matrix(NA, nrow=n, ncol=2L)
  for (k in 1:n) {
    unit_exponentials <- rexp(2L, 1)
    if (rate_drift>0.) {
      barrier_values <- sort(unit_exponentials)
    } else {
      barrier_values <- max(unit_exponentials)
    }
    d <- length(barrier_values)

    killing_waiting_time <- rexp_if_rate_zero_then_infinity(1, rate_killing)

    times <- 0.
    values <- 0.
    for (i in 1:d) {
      while (last(values) < barrier_values[i]) {
        waiting_time <- rexp_if_rate_zero_then_infinity(1, rate)

        if (killing_waiting_time < Inf && killing_waiting_time <= waiting_time) {
          for (j in i:d) {
            if (rate_drift>0. &&
                (barrier_values[[j]] - last(values))/rate_drift<=killing_waiting_time) {
              intermediate_waiting_time <- (barrier_values[[j]] - last(values)) / rate_drift
              intermediate_value <- barrier_values[[j]]
              times <- c(times, last(times) + intermediate_waiting_time)
              values <- c(values, intermediate_value)
              killing_waiting_time <- killing_waiting_time - intermediate_waiting_time
            }
          }

          times <- c(times, last(times) + killing_waiting_time)
          values <- c(values, Inf)
        } else {
          for (j in i:d) {
            if (rate_drift>0. &&
                (barrier_values[[j]] - last(values))/rate_drift <= waiting_time) {
              intermediate_waiting_time <- (barrier_values[[j]] - last(values)) / rate_drift
              intermediate_value <- barrier_values[[j]]
              times <- c(times, last(times) + intermediate_waiting_time)
              values <- c(values, intermediate_value)
              waiting_time <- waiting_time - intermediate_waiting_time
            }
          }

          if (rate>0.) { ## waiting_time<Inf # nolint
            times <- c(times, last(times) + waiting_time)
            values <- c(values, last(values) + waiting_time * rate_drift + jump_value)
          }
        }
      }
    }

    cpp_subordinator <- cbind("t"=times, "values"=values)
    out[k, ] <- vapply(1:2L, function(x) min(cpp_subordinator[cpp_subordinator[, 2] >= unit_exponentials[[x]], 1]), FUN.VALUE=0.5) # nolint
  }

  out
}





#' Alternative implementation of ind.-case via LFM
#'
#' @keywords internal
#' @noRd
test__rmo_lfm_cpp_independence_R <- function(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list) { # nolint

  out <- matrix(NA, nrow=n, ncol=d)
  for (k in 1:n) {
    unit_exponentials <- rexp(d, 1)
    out[k, ] <- unit_exponentials / rate_drift
  }

  out
}

#' Alternative implementation of com.-case via LFM
#'
#' @keywords internal
#' @noRd
test__rmo_lfm_cpp_comonotone_R <- function(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list) { # nolint

  out <- matrix(NA, nrow=n, ncol=d)
  for (k in 1:n) {
    stopifnot(all(rexp(d, 1) >= 0)) ## dummy
    out[k, ] <- rexp(1L, rate_killing)
  }

  out
}


#' Original implementation of the Compound Poisson process LFM in `R`
#'
#' @rdname rmo_lfm_cpp
#' @keywords internal
#' @noRd
test__rmo_lfm_cpp_R <- function(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list = list()) { # nolint

  out <- matrix(NA, nrow=n, ncol=d)
  for (k in 1:n) {
    unit_exponentials <- rexp(d)
    cpp_subordinator <- test__sample_cpp_R(rate, rate_killing, rate_drift,
      rjump_name, rjump_arg_list, unit_exponentials)
    out[k, ] <- vapply(1:d, function(x) {
      min(cpp_subordinator[cpp_subordinator[, 2] >= unit_exponentials[[x]], 1])
    }, FUN.VALUE=0.5)
  }

  out
}

#' @rdname rmo_lfm_cpp
#' @keywords internal
#' @noRd
test__sample_cpp_R <- function(rate, rate_killing, rate_drift, rjump_name, rjump_arg_list, barrier_values) { # nolint
  if ("rexp" == rjump_name)
    rjump <- match.fun(stats::rexp)
  else if ("rposval" == rjump_name)
    rjump <- match.fun(rposval)

  if (rate_drift>0.) {
    barrier_values <- sort(barrier_values)
  } else {
    barrier_values <- max(barrier_values)
  }
  d <- length(barrier_values)

  killing_waiting_time <- rexp_if_rate_zero_then_infinity(1, rate_killing)

  times <- 0.
  values <- 0.
  for (i in 1:d) {
    while (last(values) < barrier_values[[i]]) {
      waiting_time <- rexp_if_rate_zero_then_infinity(1, rate)
      jump_value <- do.call(rjump, args=c("n"=1, rjump_arg_list))

      if (killing_waiting_time < Inf && killing_waiting_time <= waiting_time) {
        for (j in i:d) {
          if (rate_drift>0. &&
              (barrier_values[[j]] - last(values)) /
                rate_drift<=killing_waiting_time) {
            intermediate_waiting_time <- (barrier_values[[j]] - last(values)) / rate_drift
            times <- c(times, last(times) + intermediate_waiting_time)
            values <- c(values, barrier_values[[j]])
            killing_waiting_time <- killing_waiting_time - intermediate_waiting_time
          }
        }

        times <- c(times, last(times) + killing_waiting_time)
        values <- c(values, Inf)
      } else {
        for (j in i:d) {
          if (rate_drift>0. &&
              (barrier_values[[j]] - last(values))/rate_drift <= waiting_time) {
            intermediate_waiting_time <- (barrier_values[[j]] - last(values)) / rate_drift
            times <- c(times, last(times) + intermediate_waiting_time)
            values <- c(values, barrier_values[[j]])
            waiting_time <- waiting_time - intermediate_waiting_time
          }
        }

        if (rate>0.) { ## waiting_time<Inf # nolint
          times <- c(times, last(times) + waiting_time)
          values <- c(values, last(values) + waiting_time * rate_drift + jump_value)
        }
      }
    }
  }

  cbind("t"=times, "value"=values)
}
