## #### Arnold model ####

#' Naive implementation of the Arnold model in `R`
#'
#' @keywords internal test
#' @noRd
test__rmo_arnold <- function(n, d, intensities) { # nolint
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
          if (test__is_within(i, affected)) {
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
