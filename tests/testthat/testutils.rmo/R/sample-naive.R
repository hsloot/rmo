## #### Exogenous shock model ####

#' Naive implementation in `R`
#'
#' @noRd
#' @include sample-helper.R
#' @export
rmo_esm_naive <- function(n, d, intensities) { # nolint
  out <- matrix(NA, nrow=n, ncol=d)
  for (k in 1:n) {
    ## initialise values for all components to Inf such that we can
    ## overwrite them at the first time a shock concerns this component
    value <- rep(Inf, d)
    ## iterate over all shocks
    for (j in 1:(2^d - 1)) {
      ## sample shock time
      shock_time <- rexp(1, intensities[[j]]) # nolint
      ## iterate over all components, check if current shock concerns it,
      ## and update value if that is the case.
      for (i in 1:d) {
        if (is_within(i, j))
          value[i] <- min(c(value[[i]], shock_time))
      }
    }
    out[k, ] <- value
  }

  out
}


## #### Arnold model ####

#' Naive implementation of the Arnold model in `R`
#'
#' @noRd
#' @include sample-helper.R
#' @export
rmo_arnold_naive <- function(n, d, intensities) { # nolint
  ## calculate total intensity as the sum of all shock
  ## intensities and all transition probabilities as
  ## the normalised shock intensities
  total_intensity <- sum(intensities)
  transition_probs <- intensities / total_intensity

  out <- matrix(nrow=n, ncol=d)
  for (k in 1:n) {
    ## - set all entries of `destroyed` to `FALSE`, since all
    ##   components are alive at the beginning
    ## - repeat the following steps until all components are
    ##   destroyed
    destroyed <- logical(d)
    value <- numeric(d)
    while (!all(destroyed)) {
      ## sample waiting time and the arriving shock
      waiting_time <- rexp(1, total_intensity)
      affected <- sample.int(
        n=2^d-1, size=1, replace=FALSE, prob=transition_probs)
      ## check which components will be destroyed by the
      ## current shock and increment waiting time for all
      ## components which were alive before the shock arrival
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


## #### Exchangeable MO Arnold model ####

#' Naive implementation of the exchangeable Arnold model in `R`
#'
#' @noRd
#' @include sample-helper.R
#' @export
rmo_ex_arnold_naive <- function(n, d, ex_intensities) { # nolint
  ## store total_intensity and transition_probs for all possible states
  ## (number of destroyed components) in a list
  generator_list <- list()
  for (i in 1:d) {
    transition_probs <- vapply(
      1:i,
      function(k) {
        sum(
          vapply(
            0:(d-i),
            function(l) {
              choose((d-i), l) * ex_intensities[[k + l]]
            },
            FUN.VALUE=0.5
          )
        )
      }, FUN.VALUE=0.5) *
      vapply(
        1:i,
        function(k) {
          choose(i, k)
        },
        FUN.VALUE = 0.5
      ) # intermediate result, not standardised
    total_intensity <- sum(transition_probs)
    transition_probs <- transition_probs / total_intensity
    generator_list[[i]] <- list(
      "total_intensity" = total_intensity,
      "transition_probs" = transition_probs
    )
  }

  out <- matrix(0, nrow=n, ncol=d)
  for (k in 1:n) {
    ## reset number of alive components
    d_alive <- d
    while (d_alive > 0) {
      ## retrieve total intensity and transition probabilities
      ## for current state
      total_intensity <- generator_list[[d_alive]]$total_intensity
      transition_probs <- generator_list[[d_alive]]$transition_probs

      ## sample waiting time and number of affected components
      waiting_time <- rexp(1L, total_intensity)
      num_affected <- sample.int(
        d_alive, 1, replace=FALSE, prob=transition_probs)

      ## update all components which were alive before the current
      ## shock arrived and reduce the number of alive components
      ## according to the shock size
      out[k, (d-d_alive+1):d] <- out[k, (d-d_alive+1):d] +
        waiting_time
      d_alive <- d_alive - num_affected
    }
    ## use a random permutation to reorder the components
    perm <- sample.int(d, d, replace = FALSE)
    out[k, ] <- out[k, perm]
  }

  out
}

#' Alternative implementation of the exchangeable Arnold model in `R`
#'
#' This implementation should yield the same results as the other
#' testparametrisation. Both differ in that this implementation  calculates
#' transition probabilities and intensities recursively when needed and that
#' the other one calculates them upfront.
#'
#' This implementation is instable even for mediun sized dimensions and
#' should only be used for low dimensions to test.
#'
#' @noRd
#' @include sample-helper.R
#' @export
rmo_ex_arnold_naive_recursive <- function(n, d, ex_intensities) { # nolint
  ## calculate the corresponding reparametrisation for the
  ## `ex_intensities` parameters with
  ## a[i] = sum[k=0]^[d-i-1] binom[d-i-1][k] ex_intensities[k+1] , k=0,...,d-1
  ex_a <- vapply(
    0:(d-1),
    function(i) {
      sum(
        vapply(0:(d-i-1),
               function(k) {
                 choose(d-i-1, k) * ex_intensities[[k+1]]
               },
               FUN.VALUE=0.5))
    },
    FUN.VALUE=0.5
  )

  out <- matrix(0, nrow=n, ncol=d)
  for (i in 1:n) {
    ## reset parameters at the beginning, i.e. all components are alive
    ## and we reset the `a` parameters
    ex_a_alive <- ex_a
    d_alive <- d
    while (d_alive > 0) {
      ## update the parameters
      ex_a_alive <- ex_a_alive[1:d_alive]
      ex_intensities_alive <- vapply(
        1:d_alive,
        function(i) {
          sum(
            vapply(0:(i-1),
                   function(k) {
                     (-1) ^ (k) * choose(i-1, k) * ex_a_alive[[d_alive-i+k+1]]
                   },
                   FUN.VALUE=0.5))
        },
        FUN.VALUE=0.5
      )
      ## update transition probabilities and transition intensity
      transition_probs <- vapply(
        1:d_alive,
        function(i) {
          choose(d_alive, i) * ex_intensities_alive[[i]]
        },
        FUN.VALUE=0.5
      )  # intermediate result, not normalised
      ## account for loss of significance in digits
      transition_probs <- pmax(transition_probs, 0)
      total_intensity <- sum(transition_probs)
      transition_probs <- transition_probs / total_intensity

      ## sample waiting time and transition state
      waiting_time <- rexp(1L, total_intensity)
      num_affected <- sample.int(d_alive, 1, replace=FALSE, prob=transition_probs)

      ## update all components which were alive before the current shock
      ## arrived and reduce the number of alive components
      out[i, (d-d_alive+1):d] <- out[i, (d-d_alive+1):d] + waiting_time
      d_alive <- d_alive - num_affected
    }

    ## use a random permutation to reorder the components
    perm <- sample.int(d, d, replace=FALSE)
    out[i, ] <- out[i, perm]
  }

  out
}


## #### Cuadras-Augé model ####

#' Naive implementation of the Cuadras-Augé ESM in `R`
#'
#' @noRd
#' @include sample-helper.R
#' @export
rmo_esm_cuadras_auge_naive <- function(n, d, alpha, beta) { # nolint
  out <- matrix(NA, nrow=n, ncol=d)
  for (k in 1:n) {
    ## sample the global shock
    global_shock <- rexp(1L, beta) # nolint
    ## sample the individual shocks
    individual_shocks <- rexp(d, alpha) # nolint
    out[k, ] <- pmin(individual_shocks, rep(global_shock, d))
  }

  out
}


## #### Lévy frailty model ####

#' @noRd
#' @include sample-helper.R
#' @keywords internal
sample_cpp_naive <- function( # nolint
  rate, rate_killing, rate_drift,
  rjump_name, rjump_arg_list,
  barrier_values) {
  ## get jump distribution
  if ("rexp" == rjump_name)
    rjump <- match.fun(rexp)
  else if ("rposval" == rjump_name)
    rjump <- match.fun(rposval)
  else if ("rpareto" == rjump_name)
    rjump <- match.fun(rpareto)
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
  killing_time <- rexp(1, rate_killing) # nolint

  times <- 0.
  values <- 0.
  for (i in 1:d) {
    while (last(values) < barrier_values[[i]]) {
      ## sample waiting time as well as jump value and reduce killing time
      waiting_time <- rexp(1, rate) # nolint
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

#' Naive implementation of the Compound Poisson process LFM in `R`
#'
#' @noRd
#' @include sample-helper.R
#' @export
rmo_lfm_cpp_naive <- function(
  n, d,
  rate, rate_killing, rate_drift,
  rjump_name, rjump_arg_list = list()) {
  out <- matrix(NA, nrow=n, ncol=d)
  for (k in 1:n) {
    ## sample barrier values
    unit_exponentials <- rexp(d)
    ## sample CPP path (return is array with <times, values>)
    cpp_subordinator <- sample_cpp_naive(
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
