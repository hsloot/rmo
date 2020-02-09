#' Bivariate implementation of the exchangeable Arnold model
#'
#' @keywords internal
#' @noRd
test__rmo_ex_arnold_bivariate_R <- function(n, d, ex_intensities) { # nolint
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


#' Alternative implementation of the exchangeable Arnold model in `R`
#'
#' @keywords internal
#' @noRd
test__rmo_ex_arnold_alternative_R <- function(n, d, ex_intensities) { # nolint
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


#' Original implementation of the exchangeable Arnold model in `R`
#'
#' @keywords internal
#' @noRd
test__rmo_ex_arnold_R <- function(n, d, ex_intensities) { # nolint
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
