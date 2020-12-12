## #### Exchangeable MO Arnold model ####

#' Bivariate implementation of the exchangeable Arnold model
#'
#' @keywords internal test
#' @noRd
test__rmo_ex_arnold_bivariate <- function(n, d, ex_intensities) { # nolint
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
#' @keywords internal test
#' @noRd
test__rmo_ex_arnold_alternative <- function(n, d, ex_intensities) { # nolint
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


#' Naive implementation of the exchangeable Arnold model in `R`
#'
#' @keywords internal test
#' @noRd
test__rmo_ex_arnold <- function(n, d, ex_intensities) { # nolint
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
