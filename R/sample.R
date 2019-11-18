## #### Sample from MO distributions ####
##

#' Sample from a Marshall--Olkin distribution
#'
#' Draws `n` independent samples from a `d` variate Marshall-Olkin distribution
#' with shock rates `intensities`.
#'
#' - The shock intensities must be stored in a vector of length \eqn{2^d-1}.
#' - A shock intensity of zero corresponds to an almost surely infinite shock.
#' - We use a binary representation to map a non-empty subset \eqn{I} of \eqn{\{
#' 1, \ldots, d\}}{{1, \ldots, d}} to integers \eqn{1, \ldots, 2^d-1}. In
#' particular, \eqn{i} is a component in the set corresponding for \eqn{j} iff
#' \eqn{j = \sum_{k=0}^d a_k 2^k}{\sum a[k] * 2^k} and \eqn{a_i = 0}{a[i] = 0}.
#'
#' @param n number of samples
#' @param d dimension
#' @param intensities Marshall-Olkin intensity rates
#'
#' @return `rmo_esm` implements the *exogenous shock model* representation and
#' returns an \eqn{n \times d}{n x d} numeric matrix with the rows corresponding
#' to independent and identically disctributed samples of a \eqn{d} variate
#' Marshall-Olkin distribution with parameters `intensities`.
#'
#' @section References: For more information on these algorithms, see J.-F. Mai,
#' M. Scherer, "Simulating Copulas", World Scientific (2017), pp. 104 psqq.
#'
#' @family samplers
#'
#' @examples
#' rmo_esm(10L, 2L, c(0.4, 0.3, 0.2))
#' rmo_esm(10L, 2L, c(1, 1, 0))         ## independence
#' rmo_esm(10L, 2L, c(0, 0, 1))         ## comonotone
#'
#' @include assert.R sets.R
#' @importFrom stats rexp
#' @importFrom assertthat assert_that is.count
#'
#' @export
#' @name rmo_esm
rmo_esm <- function(n, d, intensities) {
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

#' @rdname rmo_esm
#'
#' @return `rmo_arnold` implements the *Arnold model* representation and returns
#' an \eqn{n \times d}{n x d} numeric matrix with the rows corresponding to
#' independent and identically disctributed samples of a \eqn{d} variate
#' Marshall-Olkin distribution with parameters `intensities`.
#'
#' @family samplers
#'
#' @examples
#' rmo_arnold(10L, 2L, c(0.4, 0.3, 0.2))
#' rmo_arnold(10L, 2L, c(1, 1, 0))         ## independence
#' rmo_arnold(10L, 2L, c(0, 0, 1))         ## comonotone
#'
#' @include assert.R sets.R
#' @importFrom stats rexp
#' @importFrom assertthat assert_that is.count
#'
#' @export
rmo_arnold <- function(n, d, intensities) {
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


## #### Sample from exMO distribution ####
##



#' Sample from an exchangeable MO distribution
#'
#' Draws `n` independent samples from a `d` variate exchangeable Marshall-Olkin
#' distribution with shock rates `ex_intensities`.
#'
#' - The *exchangeable* shock intensities must be stored in a vector of length
#' \eqn{d}.
#' - The entry \eqn{\mathrm{ex_intensities}_{i}}{ex_intensities[i]} is the
#' intensity of a shock corresponding to a set with \eqn{i} elements.
#'
#' @section References:
#' For more information on this algorithm, see J.-F. Mai, M. Scherer,
#' "Simulating Copulas", World Scientific (2017), pp. 122 psqq.
#'
#' @param n number of samples
#' @param d dimension
#' @param ex_intensities exchangeable Marshall-Olkin intensity rates
#'
#' @return `rmo_ex_arnold` implements the modified Arnold model for the
#' exchangeable subclass and returns an \eqn{n \times d}{n x d} numeric matrix
#' with the rows corresponding to independent and identically disctributed
#' samples of a \eqn{d} variate exchangeable Marshall-Olkin distribution with
#' exchangeable parameters `ex_intensities`.
#'
#' @family samplers
#'
#' @examples
#' rmo_ex_arnold(10, 2, c(0.4, 0.2))
#' rmo_ex_arnold(10, 2, c(1, 0))      ## independence
#' rmo_ex_arnold(10, 2, c(0, 1))      ## comonotone
#'
#' @include assert.R
#' @importFrom assertthat assert_that is.count
#'
#' @export
#' @name rmo_ex_arnold
rmo_ex_arnold <- function(n, d, ex_intensities) {
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
    value <- rmo_ex_arnold_sorted(d, generator_list)
    perm <- sample.int(d, d, replace = FALSE)
    out[k, ] <- value[perm]
  }

  out
}

#' @rdname rmo_ex_arnold
#'
#' @param generator_list `list` of length `d` with the i-th element containing
#'   named elements with the `total_intensity` and `transition_probs` of the
#'   marginal exMO model mit `i` components.
#'
#' @return `rmo_ex_arnold_sorted` draws *one* sample from an ascendigly ordered
#' exchangeable Marshall--Olkin distribution and return a numeric vector of
#' length `d`.
#'
#' @family samplers
#'
#' @importFrom stats rexp
#'
#' @keywords internal
#' @noRd
rmo_ex_arnold_sorted <- function(d, generator_list) {
  total_intensity <- generator_list[[d]]$total_intensity
  transition_probs <- generator_list[[d]]$transition_probs

  waiting_time <- rexp(1, total_intensity)
  num_affected <- sample.int(d, 1, replace=FALSE, prob=transition_probs)

  if (d == num_affected) {
    return(rep(waiting_time, d))
  }

  waiting_time + c(rep(0, num_affected),
              rmo_ex_arnold_sorted(d-num_affected, generator_list))
}


## #### Sample from extMO distributions ####
##

#' Sample with LFM and CPP subordinator
#'
#' Draws `n` independent samples from a `d` variate extendible Marshall-Olkin
#' distribution corresponding to a LFM with a compound Poisson subordinator with
#' parameters `rate`, `rate_killing`, `rate_drift`, `rjump_name`, and
#' `rjump_arg_list`.
#'
#' - `rate` is the *jump intensity* of the compound Poisson subordinator:
#' at each given point-in-time, the waiting time to the next jump is
#' exponentially distributed with rate `rate`.
#' - `rate_killing` is the *killing intensity* of the compound Poisson
#' subordinator: the probability that the compound Poisson subordinator jumps to
#' its graveyard-state \eqn{\infty} between \eqn{t} and \eqn{s} is \eqn{1 -
#' \exp{\{-\mathrm{rate_killing} (s-t)\}}}{1-exp{-rate_killing * (s-t)}}.
#'
#' @param n number of samples
#' @param d dimension
#' @param rate rate of CPP subordinator
#' @param rate_killing killing rate of CPP subordinator
#' @param rate_drift drift rate of CPP subordinator
#' @param rjump_name name of jump sampling function for jumps of CPP
#' subordinator
#' @param rjump_arg_list list with named arguments for jump sampling function
#' for jumps of CPP subordinator
#'
#' @return `rmo_lfm_cpp` implements the Lévy-frailty model representation with a
#' compound Poisson subordinator and returns an \eqn{n \times d}{n x d} numeric
#' matrix with the rows corresponding to independent and identically
#' disctributed samples of the corresponding `d` variate extendible
#' Marshall-Olkin distribution.
#'
#' @section References: For more information on this algorithm, see J.-F. Mai,
#' M. Scherer, "Simulating Copulas", World Scientific (2017), pp. 140 psqq.
#'
#' @examples
#' rmo_lfm_cpp(10L, 2L, 0.5, 0.1, 0.2, "rposval", list("value"=1))
#' rmo_lfm_cpp(10L, 2L, 0.5, 0, 0, "rexp", list("rate"=2))
#' \dontrun{
#' rmo_lfm_cpp(10L, 2L, 0, 0, 1, "rposval", list("value"=1))  ## independence
#' rmo_lfm_cpp(10L, 2L, 0, 1, 0, "rposval", list("value"=1))  ## comonotone
#' }
#'
#' @family samplers
#'
#' @include assert.R
#' @importFrom assertthat assert_that is.count
#' @importFrom stats rexp
#'
#' @export
#' @name rmo_lfm_cpp
rmo_lfm_cpp <- function(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list = list()) {
  assert_that(is.count(n), is.count(d), is_positive_number(rate),
    is_nonnegative_number(rate_killing), is_nonnegative_number(rate_drift),
    is_rjump_name(rjump_name), is_rjump_arg_list(rjump_name, rjump_arg_list))

  out <- matrix(NA, nrow=n, ncol=d)
  for (k in 1:n) {
    unit_exponentials <- rexp(d)
    cpp_subordinator <- sample_cpp(rate, rate_killing, rate_drift,
      rjump_name, rjump_arg_list, max(unit_exponentials))
    out[k, ] <- vapply(1:d, function(x) min(cpp_subordinator[cpp_subordinator[, 2] >= unit_exponentials[[x]], 1]), FUN.VALUE=0.5) # nolint
  }

  out
}


#' Sample from Cuadras-Auge distribution
#'
#' Draws `n` independent samples from a `d` variate Cuadras-Augé distribution
#' with parameters `alpha` and `beta`.
#'
#' - `alpha` is the shock intensity of shocks that affect only single
#' compoenents.
#' - `beta` is the shock intensity of the global shock that affects all
#' components.
#'
#' @param n number of samples
#' @param d dimension
#' @param alpha rate of individual shocks
#' @param beta rate of global shock
#'
#' @return `rmo_esm_cuadras_auge` implements an optimized version of the
#' *exogenous shock model* representation for the Cuadras-Augé family and
#' returns an \eqn{n \times d}{n x d} array matrix with rows corresponding to
#' the independent samples of size \eqn{d}.
#'
#' @seealso \code{\link{rmo_esm}}
#' @family samplers
#'
#' @examples
#' rmo_esm_cuadras_auge(10L, 2L, 0.5, 0.2)
#' rmo_esm_cuadras_auge(10L, 2L, 0, 1)      ## comonotone
#' rmo_esm_cuadras_auge(10L, 2L, 1, 0)      ## independence
#'
#' @include assert.R
#' @importFrom assertthat assert_that is.count
#'
#' @export
#' @name rmo_esm_cuadras_auge
rmo_esm_cuadras_auge <- function(n, d, alpha, beta) {
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



## #### Auxiliary samplers ####
##

#' A wrapper for `rexp`
#'
#' Wraps an `ifelse`-clause arround `rexp` with special treatment for the case
#' `rate=0`.
#'
#' @inheritParams stats::rexp
#'
#' @importFrom stats rexp
#'
#' @keywords internal
#' @noRd
rexp_if_rate_zero_then_infinity <- function(n, rate) { # nolint
  if (rate == 0) {
    return(rep(Inf, n))
  }

  rexp(n, rate)
}

#' A dummy sampling function for deterministic, positive values
#'
#' @param n number of samples
#' @param value value to sample
#'
#' @return A `n` elements numeric vector with `value` in each component
#'
#' @examples
#' rposval(10L)       ## rep(1, 10L)
#' rposval(10L, pi)   ## rep(pi, 10L)
#'
#' @family samplers
#'
#' @keywords internal
#' @noRd
rposval <- function(n, value=1) {
  rep(value, times=n)
}


#' @rdname rmo_lfm_cpp
#'
#' A sampling function for a (possibly killed) compound Poisson subordinator
#' with non-negative jump distribution.
#'
#' @inheritParams rmo_lfm_cpp
#' @param minmax_value the smallest allowed maximal value of process
#'
#' @return A named `k x 2` array with names `c("t", "value")`, where `k` is
#' random and each row represents a time-value tupel for a jump in the compound
#' Poisson subordinator.
#'
#' @include assert.R
#' @importFrom stats rexp
#'
#' @keywords internal
#' @noRd
sample_cpp <- function(rate, rate_killing, rate_drift, rjump_name, rjump_arg_list, minmax_value) {

  times <- 0
  values <- 0
  while (sum(values) < minmax_value) {
    waiting_time <- rexp(1, rate)
    jump_value <- do.call(rjump_name, args=c("n"=1, rjump_arg_list))
    kill_value <- ifelse(rexp_if_rate_zero_then_infinity(1, rate_killing) <= waiting_time, Inf, 0)
    drift_value <- waiting_time * rate_drift

    times <- c(times, waiting_time)
    values <- c(values, kill_value + drift_value + jump_value)
  }

  cbind("t"=cumsum(times), "value"=cumsum(values))
}
