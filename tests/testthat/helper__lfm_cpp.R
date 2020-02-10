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
  stopifnot(rexp(1L, jump_rate) > 0) ## dummy

  out <- matrix(NA, nrow=n, ncol=2L)
  for (k in 1:n) {
    unit_exponentials <- rexp(2L, 1)
    if (rate_drift>0) {
      barrier_values <- sort(unit_exponentials)
    } else {
      barrier_values <- max(unit_exponentials)
    }

    times <- 0
    values <- 0
    for (i in seq_along(barrier_values)) {
      while (sum(values) < barrier_values[[i]]) {
        waiting_time <- rexp_if_rate_zero_then_infinity(1L, rate)
        jump_value <- rexp_if_rate_zero_then_infinity(1L, jump_rate)
        killing_time <- rexp_if_rate_zero_then_infinity(1L, rate_killing)

        if (killing_time < Inf && killing_time <= waiting_time) {
          if (rate_drift>0 && (barrier_values[[i]] - sum(values))/rate_drift<=killing_time) {
            intermediate_time <- (barrier_values[[i]] - sum(values)) / rate_drift
            intermediate_value <- intermediate_time * rate_drift
            times <- c(times, intermediate_time)
            values <- c(values, intermediate_value)
            killing_time <- killing_time - intermediate_time
          }

          times <- c(times, killing_time)
          values <- c(values, Inf)
        } else {
          if (rate_drift>0 && (barrier_values[[i]] - sum(values))/rate_drift <= waiting_time) {
            intermediate_time <- (barrier_values[[i]] - sum(values)) / rate_drift
            intermediate_value <- intermediate_time * rate_drift
            times <- c(times, intermediate_time)
            values <- c(values, intermediate_value)
            waiting_time <- waiting_time - intermediate_time
          }

          if (rate>0) { ## waiting_time<Inf # nolint
            times <- c(times, waiting_time)
            values <- c(values, waiting_time * rate_drift + jump_value)
          }
        }
      }
    }

    cpp_subordinator <- cbind("t"=cumsum(times), "values"=cumsum(values))
    out[k, ] <- vapply(1:2L, function(x) {
      min(cpp_subordinator[cpp_subordinator[, 2] >= unit_exponentials[[x]], 1])
    }, FUN.VALUE=0.5) # nolint
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
    if (rate_drift>0) {
      barrier_values <- sort(unit_exponentials)
    } else {
      barrier_values <- max(unit_exponentials)
    }

    times <- 0
    values <- 0
    for (i in seq_along(barrier_values)) {
      while (sum(values) < barrier_values[[i]]) {
        waiting_time <- rexp_if_rate_zero_then_infinity(1, rate)
        killing_time <- rexp_if_rate_zero_then_infinity(1, rate_killing)

        if (killing_time < Inf && killing_time <= waiting_time) {
          if (rate_drift>0 && (barrier_values[[i]] - sum(values))/rate_drift<=killing_time) {
            intermediate_time <- (barrier_values[[i]] - sum(values)) / rate_drift
            intermediate_value <- intermediate_time * rate_drift
            times <- c(times, intermediate_time)
            values <- c(values, intermediate_value)
            killing_time <- killing_time - intermediate_time
          }

          times <- c(times, killing_time)
          values <- c(values, Inf)
        } else {
          if (rate_drift>0 && (barrier_values[[i]] - sum(values))/rate_drift <= waiting_time) {
            intermediate_time <- (barrier_values[[i]] - sum(values)) / rate_drift
            intermediate_value <- intermediate_time * rate_drift
            times <- c(times, intermediate_time)
            values <- c(values, intermediate_value)
            waiting_time <- waiting_time - intermediate_time
          }
          if (rate>0) { ## waiting_time<Inf # nolint
            times <- c(times, waiting_time)
            values <- c(values, waiting_time * rate_drift + jump_value)
          }
        }
      }
    }

    cpp_subordinator <- cbind("t"=cumsum(times), "values"=cumsum(values))
    out[k, ] <- vapply(1:2L, function(x) min(cpp_subordinator[cpp_subordinator[, 2] >= unit_exponentials[[x]], 1]), FUN.VALUE=0.5) # nolint
  }

  out
}





#' Alternative implementation of ind.-case via LFM
#'
#' @keywords internal
#' @noRd
test__rmo_lfm_cpp_independence_R <- function(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list) { # nolint
  assertthat::assert_that(assertthat::is.count(n), assertthat::is.count(d),
    is_positive_number(rate_drift),
    is_rjump_name(rjump_name), is_rjump_arg_list(rjump_name, rjump_arg_list)) ## dummy test

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
  assertthat::assert_that(assertthat::is.count(n), assertthat::is.count(d),
    is_positive_number(rate_killing),
    is_rjump_name(rjump_name), is_rjump_arg_list(rjump_name, rjump_arg_list)) ## dummy test

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
  assert_that(is.count(n), is.count(d), is_nonnegative_number(rate),
    is_nonnegative_number(rate_killing), is_nonnegative_number(rate_drift),
    is_positive_number(rate + rate_killing + rate_drift),
    is_rjump_name(rjump_name), is_rjump_arg_list(rjump_name, rjump_arg_list))

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
test__sample_cpp_R <- function(rate, rate_killing, rate_drift, rjump, rjump_arg_list, barrier_values) { # nolint
  if (rate_drift>0.) {
    barrier_values <- sort(barrier_values)
  } else {
    barrier_values <- max(barrier_values)
  }

  times <- 0.
  values <- 0.
  for (i in seq_along(barrier_values)) {
    while (sum(values) < barrier_values[[i]]) {
      waiting_time <- rexp_if_rate_zero_then_infinity(1, rate)
      jump_value <- do.call(rjump, args=c("n"=1, rjump_arg_list))
      killing_time <- rexp_if_rate_zero_then_infinity(1, rate_killing)

      if (killing_time < Inf && killing_time <= waiting_time) {
        if (rate_drift>0. && (barrier_values[[i]] - sum(values))/rate_drift<=killing_time) {
          intermediate_time <- (barrier_values[[i]] - sum(values)) / rate_drift
          intermediate_value <- intermediate_time * rate_drift
          times <- c(times, intermediate_time)
          values <- c(values, intermediate_value)
          killing_time <- killing_time - intermediate_time
        }

        times <- c(times, killing_time)
        values <- c(values, Inf)
      } else {
        if (rate_drift>0. && (barrier_values[[i]] - sum(values))/rate_drift <= waiting_time) {
          intermediate_time <- (barrier_values[[i]] - sum(values)) / rate_drift
          intermediate_value <- intermediate_time * rate_drift
          times <- c(times, intermediate_time)
          values <- c(values, intermediate_value)
          waiting_time <- waiting_time - intermediate_time
        }
        if (rate>0.) { ## waiting_time<Inf # nolint
          times <- c(times, waiting_time)
          values <- c(values, waiting_time * rate_drift + jump_value)
        }
      }
    }
  }

  cbind("t"=cumsum(times), "value"=cumsum(values))
}
