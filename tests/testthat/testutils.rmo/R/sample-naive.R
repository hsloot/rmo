## #### Exogenous shock model ####

#' Naive implementation in `R`
#'
#' @param n Number of samples (> 0)
#' @param d Dimension (> 0)
#' @param intensities Shock intensities (length == 2^d-1; all >= 0, any > 0)
#'
#' @examples
#' rmo_esm_naive(10, 3, c(0.4, 0.3, 0.2, 0.5, 0.2, 0.1, 0.05))
#' rmo_esm_naive(10, 3, c(1, 1, 0, 1, 0, 0, 0)) ## independence
#' rmo_esm_naive(10, 3, c(0, 0, 0, 0, 0, 0, 1)) ## comonotone
#' @include sample-helper.R
#' @export
rmo_esm_naive <- function(n, d, intensities) { # nolint
  stopifnot(
    is.numeric(n) && 1L == length(n) && 0 == n %% 1 && n > 0 &&
    is.numeric(d) && 1L == length(d) && 0 == d %% 1 && d > 0 &&
    is.numeric(intensities) && 2^d-1 == length(intensities) &&
      all(intensities >= 0) && any(intensities > 0))

  out <- matrix(nrow = n, ncol = d)
  for (k in 1:n) {
    ## initialise values for all components to Inf such that we can
    ## overwrite them at the first time a shock concerns this component
    value <- rep(Inf, times = d)
    for (j in 1:(2^d - 1)) {
      ## sample shock time
      shock_time <- rexp(1, rate = intensities[[j]])
      ## iterate over all components, check if current shock concerns it,
      ## and update value if that is the case
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
#' @param n Number of samples (> 0)
#' @param d Dimension (> 0)
#' @param intensities Shock intensities (length == 2^d-1; all >= 0, any > 0)
#'
#' @examples
#' rmo_arnold_naive(10, 3, c(0.4, 0.3, 0.2, 0.5, 0.2, 0.1, 0.05))
#' rmo_arnold_naive(10, 3, c(1, 1, 0, 1, 0, 0, 0)) ## independence
#' rmo_arnold_naive(10, 3, c(0, 0, 0, 0, 0, 0, 1)) ## comonotone
#' @include sample-helper.R
#' @export
rmo_arnold_naive <- function(n, d, intensities) { # nolint
  stopifnot(
    is.numeric(n) && 1L == length(n) && 0 == n %% 1 && n > 0 &&
    is.numeric(d) && 1L == length(d) && 0 == d %% 1 && d > 0 &&
    is.numeric(intensities) && 2^d-1 == length(intensities) &&
      all(intensities >= 0) && any(intensities > 0))

  ## calculate arrival intensity and all arrival probabilities
  arrival_intensity <- sum(intensities)
  arrival_probs <- intensities / arrival_intensity

  out <- matrix(nrow = n, ncol = d)
  for (k in 1:n) {
    ## initialize `destroyed` and `time`
    destroyed <- rep(FALSE, times = d)
    time <- rep(0, times = d)
    while (!all(destroyed)) {
      ## while not all are destroyed, sample the waiting time and the
      ## arriving shock
      waiting_time <- rexp(1, rate = arrival_intensity)
      affected <- sample.int(n = length(arrival_probs), size = 1,
                             replace = FALSE, prob = arrival_probs)
      ## check which components will be destroyed by the shock and increment
      ## time for all components which were alive before the shock arrival
      for (i in 1:d) {
        if (!destroyed[[i]]) {
          if (is_within(i, affected)) {
            destroyed[[i]] <- TRUE
          }
          time[[i]] <- time[[i]] + waiting_time
        }
      }
    }
    out[k, ] <- time
  }

  out
}


## #### Exchangeable MO Arnold model ####

#' Naive implementation of the exchangeable Arnold model in `R`
#'
#' @param n Number of samples (> 0)
#' @param d Dimension (== 2)
#' @param ex_intensities Exchangeable shock intensities
#'   (length == 2; all >= 0, any > 0)
#'
#' @examples
#' rmo_ex_arnold_naive(10, 3, c(0.4, 0.3, 0.2))
#' rmo_ex_arnold_naive(10, 3, c(1, 0, 0)) ## independence
#' rmo_ex_arnold_naive(10, 3, c(0, 0, 1)) ## comonotone
#' @include sample-helper.R
#' @export
rmo_ex_arnold_naive <- function(n, d, ex_intensities) { # nolint
  stopifnot(
    is.numeric(n) && 1L == length(n) && 0 == n %% 1 && n > 0 &&
    is.numeric(d) && 1L == length(d) && 0 == d %% 1 && d > 0 &&
    is.numeric(ex_intensities) && d == length(ex_intensities) &&
      all(ex_intensities >= 0) && any(ex_intensities > 0))

  ## store transition intensities and transition probabilities for all possible
  ## states (number of destroyed components) in a list
  generator_list <- list()
  for (i in 1:d) {
    transition_probs <- vapply(
      1:i,
      function(k) {
        sum(
          vapply(
            0:(d - i),
            function(l) {
              choose((d - i), l) * ex_intensities[[k + l]]
            },
            FUN.VALUE = 0.5
          )
        )
      }, FUN.VALUE = 0.5) *
      vapply(
        1:i,
        function(k) {
          choose(i, k)
        },
        FUN.VALUE = 0.5
      ) # intermediate result, not standardised
    transition_intensity <- sum(transition_probs)
    transition_probs <- transition_probs / transition_intensity
    generator_list[[i]] <- list(
      "transition_intensity" = transition_intensity,
      "transition_probs" = transition_probs
    )
  }

  out <- matrix(0, nrow = n, ncol = d)
  for (k in 1:n) {
    ## reset number of alive components
    d_alive <- d
    while (d_alive > 0) {
      ## while not all are dead, retrieve transition intensity and the
      ## transition probabilities for current state
      transition_intensity <- generator_list[[d_alive]]$transition_intensity
      transition_probs <- generator_list[[d_alive]]$transition_probs

      ## sample waiting time and number of affected components
      waiting_time <- rexp(1, rate = transition_intensity)
      num_affected <- sample.int(n = d_alive, size = 1, replace = FALSE,
                                 prob = transition_probs)

      ## update all components which were alive before the current
      ## shock arrived and reduce the number of alive components
      ## according to the shock size
      out[k, (d - d_alive + 1):d] <- out[k, (d - d_alive + 1):d] + waiting_time
      d_alive <- d_alive - num_affected
    }
    ## use a random permutation to reorder the components
    perm <- sample.int(n = d, size = d, replace = FALSE)
    out[k, ] <- out[k, perm]
  }

  out
}

#' Naive (recursive) implementation of the exchangeable Arnold model in `R`
#'
#' @param n Number of samples (> 0)
#' @param d Dimension (== 2)
#' @param ex_intensities Exchangeable shock intensities
#'   (length == 2; all >= 0, any > 0)
#'
#' @details
#' This implementation should be equivalent to the other naive implementation of
#' the exchangeable Arnold model. Both differ in that this implementation
#' calculates transition probabilities and intensities recursively when needed
#' and that the other one calculates them upfront.
#'
#' This implementation is instable even for medium sized dimensions and
#' should only be used for low dimensions to test.
#'
#' @examples
#' rmo_ex_arnold_naive_recursive(10, 3, c(0.4, 0.3, 0.2))
#' rmo_ex_arnold_naive_recursive(10, 3, c(1, 0, 0)) ## independence
#' rmo_ex_arnold_naive_recursive(10, 3, c(0, 0, 1)) ## comonotone
#' @include sample-helper.R
#' @export
rmo_ex_arnold_naive_recursive <- function( # nolint
  n, d = 2, ex_intensities = c(1, 0)) {
  stopifnot(
    is.numeric(n) && 1L == length(n) && 0 == n %% 1 && n > 0 &&
    is.numeric(d) && 1L == length(d) && 0 == d %% 1 && d > 0 &&
    is.numeric(ex_intensities) && d == length(ex_intensities) &&
      all(ex_intensities >= 0) && any(ex_intensities > 0))

  ## calculate the corresponding reparametrisation for the
  ## `ex_intensities` parameters with
  ## a[i] = sum[k=0]^[d-i-1] binom[d-i-1][k] ex_intensities[k+1] , k=0,...,d-1
  ex_a <- vapply(
    0:(d - 1),
    function(i) {
      sum(
        vapply(0:(d - i - 1),
               function(k) {
                 choose(d - i - 1, k) * ex_intensities[[k + 1]]
               },
               FUN.VALUE = 0.5))
    },
    FUN.VALUE = 0.5
  )

  out <- matrix(0, nrow = n, ncol = d)
  for (i in 1:n) {
    ## reset parameters at the beginning, i.e. all components are alive
    ## and we reset the `a` parameters
    ex_a_alive <- ex_a
    d_alive <- d
    while (d_alive > 0) {
      ## update the `ex_intensities` parameters to the next submodel
      ex_a_alive <- ex_a_alive[1:d_alive]
      ex_intensities_alive <- vapply(
        1:d_alive,
        function(i) {
          sum(
            vapply(0:(i-1),
                   function(k) {
                     (-1) ^ (k) * choose(i - 1, k) *
                       ex_a_alive[[d_alive - i + k + 1]]
                   },
                   FUN.VALUE = 0.5))
        },
        FUN.VALUE = 0.5
      )
      ## update transition probabilities and transition intensity
      transition_probs <- vapply(
        1:d_alive,
        function(i) {
          choose(d_alive, i) * ex_intensities_alive[[i]]
        },
        FUN.VALUE=0.5
      )  ## intermediate result, not normalised
      ## account for possible negative values in zero tolerance
      stopifnot(all(transition_probs >= -sqrt(.Machine$double.eps)))
      transition_probs <- pmax(transition_probs, 0)
      ##  calculate transition intensity and normalize transition probabilities
      transition_intensity <- sum(transition_probs)
      transition_probs <- transition_probs / transition_intensity

      ## sample waiting time and transition state
      waiting_time <- rexp(1, rate = transition_intensity)
      num_affected <- sample.int(n = d_alive, size = 1, replace = FALSE,
                                 prob = transition_probs)

      ## update all components which were alive before the current shock
      ## arrived and reduce the number of alive components
      out[i, (d - d_alive + 1):d] <- out[i, (d - d_alive + 1):d] + waiting_time
      d_alive <- d_alive - num_affected
    }

    ## use a random permutation to reorder the components
    perm <- sample.int(n = d, size = d, replace = FALSE)
    out[i, ] <- out[i, perm]
  }

  out
}


## #### Cuadras-Augé model ####

#' Naive implementation of the Cuadras-Augé ESM in `R`
#'
#' @param n Number of samples (> 0)
#' @param d Dimension (> 0)
#' @param alpha Individual shock rate (>= 0)
#' @param beta Global shock rate (>= 0; alpha + beta > 0)
#'
#' @examples
#' rmo_esm_cuadras_auge_naive(10, 3, 0.5, 0.2)
#' rmo_esm_cuadras_auge_naive(10, 3, 0, 1)      ## comonotone
#' rmo_esm_cuadras_auge_naive(10, 3, 1, 0)      ## independence
#' @include sample-helper.R
#' @export
rmo_esm_cuadras_auge_naive <- function(n, d, alpha, beta) { # nolint
  stopifnot(
    is.numeric(n) && 1L == length(n) && 0 == n %% 1 && n > 0 &&
    is.numeric(d) && 1L == length(d) && 0 == d %% 1 && d > 0 &&
    is.numeric(alpha) && 1L == length(alpha) && alpha >= 0 &&
    is.numeric(beta) && 1L == length(beta) && beta >= 0 &&
    any(c(alpha, beta) > 0))

  out <- matrix(nrow = n, ncol = d)
  for (k in 1:n) {
    ## sample the global shock
    global_shock <- rexp(1, rate = beta)
    ## sample the individual shocks
    individual_shocks <- rexp(d, rate = alpha)
    out[k, ] <- pmin(individual_shocks, global_shock)
  }

  out
}


## #### Lévy frailty model ####

#' @param rate Intensity of compound Poisson subordinator (>= 0)
#' @param rate_killing Intensity of the killing-time (>= 0)
#' @param rate_drift Drift of the compound Poisson subordinator
#'   (>= 0; rate + rate_killing + rate_drift > 0)
#' @param rjump_name Name of the sampling function for the jumps
#'   of the compound Poisson subordinator
#' @param rjump_arg_list List with parameters for the sampling function for
#'   the jumps of the compound Poisson subordinator
#' @param barrier_values Barrier values for which the first hitting-times
#'   should be included in the returned values; hitting the largest barrier
#'   value also stops the simulation
#'
#' @noRd
#' @include sample-helper.R
#' @keywords internal
sample_cpp_naive <- function( # nolint
    rate = 0, rate_killing = 0, rate_drift = 1,
    rjump_name = "rposval", rjump_arg_list = list("values" = 0),
    barrier_values = rexp(2, rate = 1)) {
  stopifnot(
    is.numeric(rate) && 1L == length(rate) && rate >= 0 &&
    is.numeric(rate_killing) && 1L == length(rate_killing) &&
      rate_killing >= 0 &&
    is.numeric(rate_drift) && 1L == length(rate_drift) && rate_drift >= 0 &&
    any(c(rate, rate_killing, rate_drift) > 0) &&
    is.numeric(barrier_values) && length(barrier_values) > 0 &&
      all(barrier_values > 0) &&
      is.character(rjump_name) && 1L == length(rjump_name) &&
        rjump_name %in% c("rexp", "rposval", "rpareto"))

  ## get jump distribution
  rjump <- match.fun(rjump_name)
  ## sort the barrier values in ascending order
  barrier_values <- sort(barrier_values, decreasing = FALSE)
  ## if barriers cannot be surpassed by drift, only the largest barrier
  ## is relevant
  if (0 == rate_drift)
    barrier_values <- max(barrier_values)
  ## sample killing time
  killing_time <- rexp(1, rate = rate_killing)

  times <- 0
  values <- 0
  for (i in seq_length(barrier_values)) {
    while (last(values) < barrier_values[[i]]) {
      ## sample waiting time and update the waiting time to killing
      waiting_time <- rexp(1, rate = rate)
      killing_waiting_time <- killing_time - last(times)
      ## sample jump value
      jump_value <- do.call(rjump, args = c("n" = 1, rjump_arg_list))

      if (killing_waiting_time < Inf && killing_waiting_time <= waiting_time) {
        ## if killing happens before the next shock, calculate the time
        ## where individual barriers are surpassed
        for (j in i:length(barrier_values)) {
          if (rate_drift > 0 &&
              (barrier_values[[j]] - last(values)) / rate_drift <=
                killing_waiting_time) {
            ## if barrier is surpassed by drift before killing set the
            ## value accordingly
            intermediate_waiting_time <- (barrier_values[[j]] - last(values)) / rate_drift
            times <- c(times, last(times) + intermediate_waiting_time)
            values <- c(values, barrier_values[[j]])
            killing_waiting_time <- killing_waiting_time - intermediate_waiting_time
          }
        }

        ## all remaining barriers will be surpassed when killing happens
        times <- c(times, last(times) + killing_waiting_time)
        values <- c(values, Inf)
      } else {
        ## if killing does not happen before the next shock, calculate the time
        ## where individual barriers are surpassed
        for (j in i:length(barrier_values)) {
          if (rate_drift > 0 &&
              (barrier_values[[j]] - last(values)) / rate_drift <=
                waiting_time) {
            ## if killing does not happen before the next shock, but barrier is
            ## surpassed by drift, set the value accordingly
            intermediate_waiting_time <- (barrier_values[[j]] - last(values)) /
                                          rate_drift
            times <- c(times, last(times) + intermediate_waiting_time)
            values <- c(values, barrier_values[[j]])
            waiting_time <- waiting_time - intermediate_waiting_time
          }
        }

        if (waiting_time < Inf) { ##  rate > 0 # nolint
          ## if the waiting time is not infinity, include the jump in
          ## the result set
          times <- c(times, last(times) + waiting_time)
          values <- c(
            values,
            last(values) + waiting_time * rate_drift + jump_value
          )
        }
      }
    }
  }

  cbind("t"=times, "value"=values)
}

#' Naive implementation of the compound Poisson process LFM in `R`
#'
#' @param n Number of samples (> 0)
#' @param d Dimension (> 0)
#' @param rate Intensity of compound Poisson subordinator (>= 0)
#' @param rate_killing Intensity of the killing-time (>= 0)
#' @param rate_drift Drift of the compound Poisson subordinator
#'   (>= 0; rate + rate_killing + rate_drift > 0)
#' @param rjump_name Name of the sampling function for the jumps
#'   of the compound Poisson subordinator
#' @param rjump_arg_list List with parameters for the sampling function for
#'   the jumps of the compound Poisson subordinator
#'
#' @examples
#' rmo_lfm_cpp_naive(10, 3, 0.5, 0.1, 0.2, "rposval", list("value"=1))
#' rmo_lfm_cpp_naive(10, 3, 0.5, 0, 0, "rexp", list("rate"=2))
#'
#' ## independence
#' rmo_lfm_cpp_naive(10, 3, 0, 0, 1, "rposval", list("value"=1))
#' ## comonotone
#' rmo_lfm_cpp_naive(10, 3, 0, 1, 0, "rposval", list("value"=1))
#' @include sample-helper.R
#' @export
rmo_lfm_cpp_naive <- function( # nolint
    n, d = 2,
    rate = 0, rate_killing = 0, rate_drift = 1,
    rjump_name = "rposval", rjump_arg_list = list("value" = 0)) {
  stopifnot(
    is.numeric(n) && 1L == length(n) && 0 == n %% 1 && n > 0 &&
    is.numeric(d) && 1L == length(d) && 0 == d %% 1 && d > 0 &&
    is.numeric(rate) && 1L == length(rate) && rate >= 0 &&
    is.numeric(rate_killing) && 1L == length(rate_killing) &&
      rate_killing >= 0 &&
    is.numeric(rate_drift) && 1L == length(rate_drift) && rate_drift >= 0 &&
    any(c(rate, rate_killing, rate_drift) > 0) &&
    is.character(rjump_name) && 1L == length(rjump_name) &&
      rjump_name %in% c("rexp", "rposval", "rpareto"))

  out <- matrix(nrow = n, ncol = d)
  for (k in 1:n) {
    ## sample unit exponential barrier values
    barrier_values <- rexp(d, rate = 1)
    ## sample CPP path including minimal times where barriers are surpassed
    ## (return is array with <"t = times", "value" = values>)
    cpp_subordinator <- sample_cpp_naive(
      rate = rate, rate_killing = rate_killing, rate_drift = rate_drift,
      rjump_name = rjump_name, rjump_arg_list = rjump_arg_list,
      barrier_values = barrier_values)
    ## find the smallest time-index value for which the
    ## subordinator surpasses the respective barriers
    times <- cpp_subordinator[, "t"]
    values <- cpp_subordinator[, "value"]
    out[k, ] <- vapply(
      1:d,
      function(x) {
        min(times[values >= barrier_values[[x]]])
      },
      FUN.VALUE=0.5
    )
  }

  out
}
