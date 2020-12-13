## #### Lévy frailty model ####

#' Alternative implementation of ind.-case via LFM
#'
#' @param n Number of samples (> 0)
#' @param d Dimension (> 0)
#' @param rate Intensity of compound Poisson subordinator (== 0)
#' @param rate_killing Intensity of the killing-time (== 0)
#' @param rate_drift Drift of the compound Poisson subordinator (> 0)
#' @param rjump_name Name of the sampling function for the jumps
#'   of the compound Poisson subordinator
#' @param rjump_arg_list List with parameters for the sampling function for
#'   the jumps of the compound Poisson subordinator
#'
#' @examples
#' rmo_lfm_cpp_independence(10, 3, 0, 0, 1, "rposval", list("value"=1))
#' @include sample-helper.R
#' @export
rmo_lfm_cpp_independence <- function(
    n, d = 2,
    rate = 0, rate_killing = 0, rate_drift = 1,
    rjump_name = "rposval", rjump_arg_list = list("value" = 0)) {
  stopifnot(
    is.numeric(n) && 1L == length(n) && 0 == n %% 1 && n > 0 &&
    is.numeric(d) && 1L == length(d) && 0 == d %% 1 && d > 0 &&
    is.numeric(rate) && 1L == length(rate) && rate == 0 &&
    is.numeric(rate_killing) && 1L == length(rate_killing) &&
      rate_killing == 0 &&
    is.numeric(rate_drift) && 1L == length(rate_drift) && rate_drift > 0 &&
    is.character(rjump_name) && 1L == length(rjump_name) &&
      rjump_name %in% c("rexp", "rposval", "rpareto"))

  out <- matrix(NA, nrow=n, ncol=d)
  for (k in 1:n) {
    ## sample unit exponential barrier values
    barrier_values <- rexp(d, rate = 1)
    ## the return values are the times a pure-drift subordinator surpasses
    ## these barrier values
    out[k, ] <- barrier_values / rate_drift
  }

  out
}
