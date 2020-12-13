## #### Exogenous shock model ####

#' Bivariate implementation
#'
#' @param n Number of samples (> 0)
#' @param d Dimension (== 2)
#' @param intensities Shock intensities (length == 3; all >= 0, any > 0)
#'
#' @examples
#' rmo_esm_bivariate(10, 2, c(0.4, 0.3, 0.2))
#' rmo_esm_bivariate(10, 2, c(1, 1, 0)) ## independence
#' rmo_esm_bivariate(10, 2, c(0, 0, 1)) ## comonotone
#' @include sample-helper.R
#' @export
rmo_esm_bivariate <- function(n, d = 2, intensities = c(1, 1, 0)) {
  stopifnot(
    is.numeric(n) && 1L == length(n) && 0 == n %% 1 && n > 0 &&
    is.numeric(d) && 1L == length(d) && 0 == d %% 1 && d == 2 &&
    is.numeric(intensities) && 3 == length(intensities) &&
      all(intensities >= 0) && any(intensities > 0))

  first_intensity <- intensities[[1]]
  second_intensity <- intensities[[2]]
  combined_intensity <- intensities[[3]]

  out <- matrix(nrow = n, ncol = 2)
  for (i in 1:n) {
    ## individual shock for component 1
    first <- rexp(1, first_intensity)
    ## individual shock for component 2
    second <- rexp(1, second_intensity)
    ## global shock for both components, 1 and 2
    combined <- rexp(1, combined_intensity)

    out[i, ] <- pmin(c(first, second), combined)
  }

  out
}

## #### Exchangeable MO Arnold model ####

#' Bivariate implementation of the exchangeable Arnold model
#'
#' @param n Number of samples (> 0)
#' @param d Dimension (== 2)
#' @param ex_intensities Exchangeable shock intensities
#'   (length == 2; all >= 0, any > 0)
#'
#' @examples
#' rmo_ex_arnold_bivariate(10, 2, c(0.4, 0.3))
#' rmo_ex_arnold_bivariate(10, 2, c(1, 0)) ## independence
#' rmo_ex_arnold_bivariate(10, 2, c(0, 1)) ## comonotone
#' @include sample-helper.R
#' @export
rmo_ex_arnold_bivariate <- function(n, d = 2, ex_intensities = c(1, 0)) {
  stopifnot(
    is.numeric(n) && 1L == length(n) && 0 == n %% 1 && n > 0 &&
    is.numeric(d) && 1L == length(d) && 0 == d %% 1 && d == 2 &&
    is.numeric(ex_intensities) && 2 == length(ex_intensities) &&
      all(ex_intensities >= 0) && any(ex_intensities > 0))

  ## calculate the transition intensities for all states
  first_transition_intensity <- 2 * ex_intensities[[1]] + ex_intensities[[2]]
  second_transition_intensity <- ex_intensities[[1]] + ex_intensities[[2]]
  ## calculate the transition probabilities for all states
  first_transition_prob <- c(2 * ex_intensities[[1]], ex_intensities[[2]]) /
    first_transition_intensity

  out <- matrix(nrow = n, ncol = 2)
  for (k in 1:n) {
    ## sample waiting time and cardinality of next arriving shock
    waiting_time <- rexp(1, rate = first_transition_intensity)
    num_affected <- sample.int(2, 1, replace = FALSE,
                               prob = first_transition_prob)
    out[k, ] <- waiting_time

    if (num_affected < 2) {
      ## if less than two components are affected sample another waiting time
      ## and set the value of the second component accordingly
      waiting_time <- rexp(1, rate = second_transition_intensity)
      out[k, 2] <- out[k, 2] + waiting_time
      # we do not need it here, but we have to sample another random integer
      # to keep the random number generators in sync
      num_affected <- sample.int(1, 1, replace = FALSE) ## dummy
    }
    ## use a random permutation to reorder the components
    perm <- sample.int(2, 2, replace = FALSE)
    out[k, ] <- out[k, perm]
  }
  out
}


## #### Cuadras-Augé model ####

#' Bivariate implementatino of the Cuadras-Augé ESM
#'
#' @param n Number of samples (> 0)
#' @param d Dimension (== 2)
#' @param alpha Individual shock rate (>= 0)
#' @param beta Global shock rate (>= 0; alpha + beta > 0)
#'
#' @examples
#' rmo_esm_cuadras_auge_bivariate(10, 2, 0.5, 0.2)
#' rmo_esm_cuadras_auge_bivariate(10, 2, 0, 1)      ## comonotone
#' rmo_esm_cuadras_auge_bivariate(10, 2, 1, 0)      ## independence
#' @include sample-helper.R
#' @export
rmo_esm_cuadras_auge_bivariate <- function(n, d = 2, alpha = 1, beta = 0) {
  stopifnot(
    is.numeric(n) && 1L == length(n) && 0 == n %% 1 && n > 0 &&
    is.numeric(d) && 1L == length(d) && 0 == d %% 1 && d == 2 &&
    is.numeric(alpha) && 1L == length(alpha) && alpha >= 0 &&
    is.numeric(beta) && 1L == length(beta) && beta >= 0 &&
    any(c(alpha, beta) > 0))

  out <- matrix(nrow = n, ncol = 2)
  for (k in 1:n) {
    ## sample the global shock
    global_shock <- rexp(1, rate = beta)
    ## sample the individual shocks
    individual_shock_1 <- rexp(1, rate = alpha)
    individual_shock_2 <- rexp(1, rate = alpha)
    out[k, ] <- pmin(c(individual_shock_1, individual_shock_2), global_shock)
  }

  out
}
