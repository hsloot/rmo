#' Bivariate implementation of the Arnold model
#'
#' @rdname rmo_arnold
#' @keywords internal
#' @noRd
test__rmo_arnold_bivariate_R <- function(n, d, intensities) { # nolint
  total_intensity <- sum(intensities)
  transition_probs <- intensities / total_intensity

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


#' Original implementation of the Arnold model in `R`
#'
#' @rdname rmo_arnold
#' @keywords internal
#' @noRd
test__rmo_arnold_R <- function(n, d, intensities) { # nolint
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
          if (test__is_within_R(i, affected)) {
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
