## File contains test functions for various simulation Marshall--Olkin
## simulation algorithms.
##
## Naming convention for the functions: test__rmo_<name>_<specialisation>_R
##
## #### Setup ####
##
if (!"assertthat" %in% .packages()) {
  library("assertthat", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
}
## Use pre R 3.6.x sample RNG since the new one is not yet
## implemented in Rcpp.
suppressWarnings(RNGkind(sample.kind="Rounding"))

## #### Bivariate implementations ####
##

#' Bivariate exogenous shock model
#'
#' A simple implementation of the bivariate exogenous shock model
#' algorithm.
#'
#' @param n number of simulations
#' @param intensities shock model intensity rates
#'
#' @returns A \eqn{n \times 2} numeric array with the simulation
#' 	results.
#'
#' @importFrom stats rexp
#'
#' @keywords internal
#' @noRd
test__rmo_esm_bivariate_R <- function(n, d, intensities) { # nolint
  assertthat::assert_that(assertthat::is.count(n), assertthat::is.count(d),
    d == 2L, is_mo_parameter(intensities), length(intensities) == 2^d-1)

  out <- matrix(0, nrow = n, ncol = 2)
    for (i in 1:n) {
      shock_for_1 <- rexp_if_rate_zero_then_infinity(1, intensities[[1]])
      shock_for_2 <- rexp_if_rate_zero_then_infinity(1, intensities[[2]])
      shock_for_1_and_2 <- rexp_if_rate_zero_then_infinity(1, intensities[[3]])
      out[i, ] <- pmin(c(shock_for_1, shock_for_2), shock_for_1_and_2)
    }

  out
}

#' Bivariate Arnold model
#'
#' A simple implementation of the bivariate Arnold
#' model algorithm.
#'
#' @param n number of simulations
#' @param intensities shock model intensity rates
#'
#' @returns A \eqn{n \times 2} numeric array with the simulation
#' 	results.
#'
#' @importFrom stats rexp
#'
#' @keywords internal
#' @noRd
test__rmo_arnold_bivariate_R <- function(n, d, intensities) { # nolint
  assertthat::assert_that(assertthat::is.count(n), assertthat::is.count(d),
    d == 2L, is_mo_parameter(intensities), length(intensities) == 2^d-1)

  total_intensity <- sum(intensities)
  transition_probs <-intensities / total_intensity

  out <- matrix(0, nrow=n, ncol=2)
  for (i in 1:n) {
  destroyed <- rep(FALSE, 2)
    while (!all(destroyed)) {
      waiting_time <- rexp(1, total_intensity)
      affected <- sample.int(3, 1, replace=FALSE, prob=transition_probs)
      out[i, !destroyed] <- out[i, !destroyed] + waiting_time
      if (affected == 1) {
        destroyed[1] <- TRUE
      } else if (affected == 2) {
        destroyed[2] <- TRUE
      } else {
        destroyed <- rep(TRUE, 2)
      }
    }
  }

  out
}


#' Bivariate exchangeable Arnold model
#'
#' A simple implementation of the modified bivariate Arnold
#' model algorithm for the exchangeable shubclass.
#'
#' @param n number of simulations
#' @param ex_intensities shock model intensity rates
#'
#' @returns A \eqn{n \times 2} numeric array with the simulation
#' 	results.
#'
#' @importFrom stats rexp
#'
#' @keywords internal
#' @noRd
test__rmo_ex_arnold_bivariate_R <- function(n, d, ex_intensities) { # nolint
  assertthat::assert_that(assertthat::is.count(n), assertthat::is.count(d),
    d == 2L, is_exmo_parameter(ex_intensities), length(ex_intensities) == d)

  total_intensity <- 2*ex_intensities[[1]] + ex_intensities[[2]]
  transition_probs <- c(2*ex_intensities[[1]], ex_intensities[[2]]) /
    total_intensity

  out <- matrix(0, nrow=n, ncol=2)
  for (i in 1:n) {
    waiting_time <- rexp(1, total_intensity)
    num_affected <- sample.int(2, 1, replace=FALSE, prob=transition_probs)

    if (num_affected < 2) {
      out[i, 1] <- waiting_time

      waiting_time <- rexp(1, sum(ex_intensities))
      num_affected <- sample.int(1, 1, replace=FALSE) # dummy
      out[i, 2] <- out[i, 1] + waiting_time

      perm <- sample.int(2, 2, replace=FALSE)
      out[i, ] <- out[i, perm]
    } else {
      out[i, ] <- waiting_time
      perm <- sample.int(2, 2, replace=FALSE) # dummy
    }
  }

  out
}

#' Bivariate LFM-CPP
#'
#' A wrapper for bivariate implementation of the Lévy-frailty model
#' with a compound Poisson subordinator and specific jump distributions
#' for the extendible subclass.
#'
#' @inheritParams rmo_lfm_cpp
#'
#' @return A `n x 2` array with samples from the corresponding bivariate
#'  Marshall-Olkin distributino.
#'
#' @importFrom assertthat assert_that is.count
#'
#' @keywords internal
#' @noRd
test__rmo_lfm_cpp_bivariate_R <- function(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list) { # nolint
  assertthat::assert_that(assertthat::is.count(n), assertthat::is.count(d),
    is_nonnegative_number(rate), is_nonnegative_number(rate_killing),
    is_nonnegative_number(rate_drift),
    is_positive_number(rate + rate_killing + rate_drift),
    is_rjump_name(rjump_name),
    is_rjump_arg_list(rjump_name, rjump_arg_list), d == 2L)

    if (rjump_name == "rexp") {
      return(test__rmo_lfm_cpp_bivariate_rexp_R(n, rate, rate_killing,
        rate_drift, rjump_arg_list$rate))
    } else if (rjump_name == "rposval") {
      return(test__rmo_lfm_cpp_bivariate_rposval_R(n, rate, rate_killing,
        rate_drift, rjump_arg_list$value))
    }
}

#' @rdname test__rmo_lfm_cpp_bivariate_R
#'
#' @importFrom stats rexp
#'
#' @keywords internal
#' @noRd
test__rmo_lfm_cpp_bivariate_rexp_R <- function(n, rate, rate_killing, rate_drift, jump_rate) { # nolint
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
      while (sum(values) < barrier_values[i]) {
        waiting_time <- rexp_if_rate_zero_then_infinity(1, rate)
        jump_value <- rexp_if_rate_zero_then_infinity(1, jump_rate)
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

    cpp_subordinator <- cbind("t"=cumsum(times), "values"=cumsum(values))
    out[k, ] <- vapply(1:2L, function(x) min(cpp_subordinator[cpp_subordinator[, 2] >= unit_exponentials[[x]], 1]), FUN.VALUE=0.5) # nolint
  }

  out
}

#' @rdname test__rmo_lfm_cpp_bivariate_R
#'
#' @importFrom stats rexp
#'
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
      while (sum(values) < barrier_values[i]) {
        waiting_time <- rexp_if_rate_zero_then_infinity(1, rate)
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

    cpp_subordinator <- cbind("t"=cumsum(times), "values"=cumsum(values))
    out[k, ] <- vapply(1:2L, function(x) min(cpp_subordinator[cpp_subordinator[, 2] >= unit_exponentials[[x]], 1]), FUN.VALUE=0.5) # nolint
  }

  out
}


#' Bivariate Cuadras-Augé
#'
#' An implementation of the bivariate Cuadras-Augé model as a specialised
#' version of the exogenous shock model
#'
#' @inheritParams rmo_esm_cuadras_auge
#'
#' @return A `n x 2` array with samples from the corresponding bivariate
#'  Marshall-Olkin distributino.
#'
#' @include assert.R
#' @importFrom assertthat assert_that is.count
#'
#' @keywords internal
#' @noRd
test__rmo_esm_cuadras_auge_bivariate_R <- function(n, d, alpha, beta) { # nolint
  assert_that(is.count(n), is.count(d), is_nonnegative_number(alpha),
    is_nonnegative_number(beta), alpha + beta > 0, d == 2L)

  out <- matrix(NA, nrow=n, ncol=2L)
  for (k in 1:n) { # use rexp_if_rate_zero_then_infinity from `R/sample.R`
    individual_shock_1 <- rexp_if_rate_zero_then_infinity(1L, alpha)
    individual_shock_2 <- rexp_if_rate_zero_then_infinity(1L, alpha)
    global_shock <- rexp_if_rate_zero_then_infinity(1L, beta)
    out[k, ] <- pmin(c(individual_shock_1, individual_shock_2), rep(global_shock, 2L))
  }

  out
}


## #### Specialised implementations ####
##

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

## #### Altenative implementations ####

#' Alternative implementation of exchangeable Arnold model
#'
#' A simple implementation of the modified bivariate Arnold
#' model algorithm for the exchangeable shubclass.
#'
#' @param n number of simulations
#' @param d dimension of MO distribution
#' @param ex_intensities shock model intensity rates
#'
#' @returns A \eqn{n \times d} numeric array with the simulation
#' 	results.
#'
#' @importFrom stats rexp
#'
#' @keywords internal
#' @noRd
test__rmo_ex_arnold_alternative_R <- function(n, d, ex_intensities) { # nolint
  assertthat::assert_that(assertthat::is.count(n), assertthat::is.count(d),
    is_exmo_parameter(ex_intensities), length(ex_intensities) == d)

  ex_a <- vapply(0:(d-1), function(x) sum(vapply(0:(d-x-1), function(y) choose(d-x-1, y) * ex_intensities[[y+1]], FUN.VALUE=0.5)), FUN.VALUE=0.5) # nolint

  out <- matrix(0, nrow=n, ncol=d)

  for (i in 1:n) {
    ex_a_tmp <- ex_a
    d_tmp <- d
    while (d_tmp > 0) {
      ex_a_tmp <- ex_a_tmp[1:d_tmp]
      ex_intensities_tmp <- vapply(1:d_tmp, function(x) sum(vapply(0:(x-1), function(y) (-1)^(y) * choose(x-1, y) * ex_a_tmp[[d_tmp-x+y+1]], FUN.VALUE=0.5)), FUN.VALUE=0.5) # nolint

      transition_probs <- vapply(1:d_tmp, function(x) choose(d_tmp, x), FUN.VALUE=0.5) *
        ex_intensities_tmp # intermediate result
      total_intensity <- sum(transition_probs)
      transition_probs <- transition_probs / total_intensity

      waiting_time <- rexp(1, total_intensity)
      num_affected <- sample.int(d_tmp, 1, replace=FALSE, prob=transition_probs)

      out[i, (d-d_tmp+1):d] <- out[i, (d-d_tmp+1):d] + waiting_time
      d_tmp <- d_tmp - num_affected
    }

    perm <- sample.int(d, d, replace=FALSE)
    out[i, ] <- out[i, perm]
  }

  out
}



## #### Old implementations in R ####
##

#' @keywords internal
#' @noRd
test__rmo_esm_R <- function(n, d, intensities) { # nolint
  assert_that(is.count(n), is.count(d), is_mo_parameter(intensities),
    length(intensities) == 2^d-1)

  out <- matrix(NA, nrow=n, ncol=d)
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


#' @keywords internal
#' @noRd
test__rmo_arnold_R <- function(n, d, intensities) { # nolint
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


#' @keywords internal
#' @noRd
test__rmo_ex_arnold_R <- function(n, d, ex_intensities) { # nolint
  assert_that(is.count(n), is.count(d), is_exmo_parameter(ex_intensities),
    length(ex_intensities) == d)

  ## store total_intensity and transition_probs for all possible states
  ## (number of destroyed components) in a list
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
    value <- test__rmo_ex_arnold_sorted_R(d, generator_list)
    perm <- sample.int(d, d, replace = FALSE)
    out[k, ] <- value[perm]
  }

  out
}


#' @keywords internal
#' @noRd
test__rmo_ex_arnold_sorted_R <- function(d, generator_list) { # nolint
  total_intensity <- generator_list[[d]]$total_intensity
  transition_probs <- generator_list[[d]]$transition_probs

  waiting_time <- rexp(1, total_intensity)
  num_affected <- sample.int(d, 1, replace=FALSE, prob=transition_probs)

  if (d == num_affected) {
    return(rep(waiting_time, d))
  }

  waiting_time + c(rep(0, num_affected),
              test__rmo_ex_arnold_sorted_R(d-num_affected, generator_list))
}




#' @keywords internal
#' @noRd
test__rmo_esm_cuadras_auge_R <- function(n, d, alpha, beta) { # nolint
  assert_that(is.count(n), is.count(d), is_nonnegative_number(alpha),
    is_nonnegative_number(beta), alpha + beta > 0)

  out <- matrix(NA, nrow=n, ncol=d)
  for (k in 1:n) { # use rexp_if_rate_zero_then_infinity from `R/sample.R`
    individual_shocks <- rexp_if_rate_zero_then_infinity(d, alpha)
    global_shock <- rexp_if_rate_zero_then_infinity(1L, beta)
    out[k, ] <- pmin(individual_shocks, rep(global_shock, d))
  }

  out
}
