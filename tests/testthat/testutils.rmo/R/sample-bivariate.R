## #### Exogenous shock model ####

#' Bivariate implementation
#'
#' @noRd
#' @include sample-helper.R
#' @export
rmo_esm_bivariate <- function(n, d, intensities) { # nolint
  out <- matrix(0, nrow = n, ncol = 2)
  for (i in 1:n) {
    ## individual shock for component 1
    first <- rexp(1, intensities[[1]]) # nolint
    ## individual shock for component 2
    second <- rexp(1, intensities[[2]]) # nolint
    ## global shock for both components
    combined <- rexp(1, intensities[[3]]) # nolint

    out[i, ] <- pmin(c(first, second), combined)
  }

  out
}


## #### Arnold model ####


## #### Exchangeable MO Arnold model ####

#' Bivariate implementation of the exchangeable Arnold model
#'
#' @noRd
#' @include sample-helper.R
#' @export
rmo_ex_arnold_bivariate <- function(n, d, ex_intensities) { # nolint
  ## calculate intensities and transition probabilities for
  ## cardinality shocks
  total_intensity <- 2*ex_intensities[[1]] + ex_intensities[[2]]
  transition_probs <- c(2*ex_intensities[[1]], ex_intensities[[2]]) /
    total_intensity

  out <- matrix(0, nrow=n, ncol=2)
  for (k in 1:n) {
    ## sample waiting time and cardinality of next arriving shock
    waiting_time <- rexp(1, total_intensity)
    num_affected <- sample.int(2, 1, replace=FALSE, prob=transition_probs)

    if (num_affected < 2) {
      ## if less than two components are affected
      ## - set the value of the corresponding component to
      ##   the waiting time
      ## - sample another pair of waiting time and shock
      ##   cardinatility (which must be equal to 2) and
      ##   set the value of the second component accordingly
      out[k, 1] <- waiting_time

      waiting_time <- rexp(1, sum(ex_intensities))
      num_affected <- sample.int(1, 1, replace=FALSE) # dummy
      out[k, 2] <- out[k, 1] + waiting_time
    } else {
      ## if two components are affected, set the values of both
      ## components to the waiting time
      out[k, ] <- waiting_time
    }
    ## use a random permutation to reorder the components
    perm <- sample.int(2, 2, replace=FALSE)
    out[k, ] <- out[k, perm]
  }
  out
}


## #### Cuadras-Augé model ####

#' Bivariate implementatino of the Cuadras-Augé ESM
#'
#' @noRd
#' @include sample-helper.R
#' @export
rmo_esm_cuadras_auge_bivariate <- function(n, d, alpha, beta) { # nolint
  out <- matrix(NA, nrow=n, ncol=2L)
  for (k in 1:n) {
    ## sample the global shock
    global_shock <- rexp(1L, beta) # nolint
    ## sample the individual shocks
    individual_shock_1 <- rexp(1L, alpha) # nolint
    individual_shock_2 <- rexp(1L, alpha) # nolint
    out[k, ] <- pmin(
      c(individual_shock_1, individual_shock_2),
      rep(global_shock, 2L))
  }

  out
}
