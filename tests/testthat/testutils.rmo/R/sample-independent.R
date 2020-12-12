## #### Lévy frailty model ####

#' Alternative implementation of ind.-case via LFM
#'
#' @noRd
#' @include sample-helper.R
#' @export
rmo_lfm_cpp_independence <- function(
  n, d,
  rate, rate_killing, rate_drift,
  rjump_name, rjump_arg_list) {
  out <- matrix(NA, nrow=n, ncol=d)
  for (k in 1:n) {
    ## sample barriers and the values when they are surpassed by
    ## the drift
    unit_exponentials <- rexp(d, 1)
    out[k, ] <- unit_exponentials / rate_drift
  }

  out
}
