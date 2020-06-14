#' Alternative implementation of ind.-case via LFM
#'
#' @keywords internal
#' @noRd
test__rmo_lfm_cpp_independence <- function(
    n, d,
    rate, rate_killing, rate_drift,
    rjump_name, rjump_arg_list) {
  out <- matrix(NA, nrow=n, ncol=d)
  for (k in 1:n) {
    ## sample barriers and the values when they are surpassed by
    ## the drift
    unit_exponentials <- rexp(d, 1)
    out[k, ] <- unit_exponentials / rate_drift
  }

  out
}

#' Alternative implementation of com.-case via LFM
#'
#' @keywords internal
#' @noRd
test__rmo_lfm_cpp_comonotone <- function(
    n, d,
    rate, rate_killing, rate_drift,
    rjump_name, rjump_arg_list) {
  out <- matrix(NA, nrow=n, ncol=d)
  for (k in 1:n) {
    stopifnot(all(rexp(d, 1) >= 0)) ## dummy
    ## sample killing time
    killing_time <- rexp(1L, rate_killing)
    out[k, ] <- killing_time
  }

  out
}


#' Original implementation of the Compound Poisson process LFM in `R`
#'
#' @rdname rmo_lfm_cpp
#' @keywords internal
#' @noRd
test__rmo_lfm_cpp <- function(
    n, d,
    rate, rate_killing, rate_drift,
    rjump_name, rjump_arg_list = list()) {
  out <- matrix(NA, nrow=n, ncol=d)
  for (k in 1:n) {
    ## sample barrier values
    unit_exponentials <- rexp(d)
    ## sample CPP path (return is array with <times, values>)
    cpp_subordinator <- test__sample_cpp(
      rate, rate_killing, rate_drift,
      rjump_name, rjump_arg_list,
      unit_exponentials)
    ## find the smallest time-index value for which the
    ## subordinator surpasses the respective barriers
    times <- cpp_subordinator[, "t"]
    values <- cpp_subordinator[, "value"]
    out[k, ] <- vapply(
      1:d,
      function(x) {
        min(times[values >= unit_exponentials[[x]]])
      },
      FUN.VALUE=0.5
    )
  }

  out
}

#' @rdname rmo_lfm_cpp
#' @keywords internal
#' @noRd
test__sample_cpp <- function( # nolint
    rate, rate_killing, rate_drift,
    rjump_name, rjump_arg_list,
    barrier_values) {
  ## get jump distribution
  if ("rexp" == rjump_name)
    rjump <- match.fun(stats::rexp)
  else if ("rposval" == rjump_name)
    rjump <- match.fun(rposval)
  else
    stop(sprintf("%s not implemented", rjump_name))

  ## sort or maximise the barrier values (we only need to account for the
  ## specific barrier values if a drift is present)
  if (rate_drift>0.) {
    barrier_values <- sort(barrier_values)
  } else {
    barrier_values <- max(barrier_values)
  }
  d <- length(barrier_values)

  ## sample killing time
  killing_time <- rexp_(1, rate_killing) # nolint

  times <- 0.
  values <- 0.
  for (i in 1:d) {
    while (last(values) < barrier_values[[i]]) {
      ## sample waiting time as well as jump value and reduce killing time
      waiting_time <- rexp_(1, rate) # nolint
      killing_waiting_time <- killing_time - last(times)
      jump_value <- do.call(rjump, args=c("n"=1, rjump_arg_list))

      if (killing_waiting_time < Inf && killing_waiting_time <= waiting_time) {
        ## if killing happends before the next shock, calculate the time
        ## where individual barriers are surpassed
        for (j in i:d) {
          if (rate_drift>0. &&
              (barrier_values[[j]] - last(values)) /
                rate_drift<=killing_waiting_time) {
            ## if barrier is surpassed by drift before killing set the
            ## value accordingly
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
            ## if killing does not happen before the next shock, but barrier is
            ## surpassed by drift, set the value accordingly
            intermediate_waiting_time <- (barrier_values[[j]] - last(values)) / rate_drift
            times <- c(times, last(times) + intermediate_waiting_time)
            values <- c(values, barrier_values[[j]])
            waiting_time <- waiting_time - intermediate_waiting_time
          }
        }

        if (rate>0.) { ## waiting_time<Inf # nolint
          ## otherwise update the component values with the value of
          ## the subordinator at time + waiting_time
          times <- c(times, last(times) + waiting_time)
          values <- c(values, last(values) + waiting_time * rate_drift + jump_value)
        }
      }
    }
  }

  cbind("t"=times, "value"=values)
}
