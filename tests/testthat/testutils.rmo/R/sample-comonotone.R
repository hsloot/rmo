## #### Lévy frailty model ####

#' Alternative implementation of com.-case via LFM
#'
#' @noRd
#' @include sample-helper.R
#' @export
rmo_lfm_cpp_comonotone <- function(
  n, d,
  rate, rate_killing, rate_drift,
  rjump_name, rjump_arg_list) {
  out <- matrix(NA, nrow=n, ncol=d)
  for (k in 1:n) {
    stopifnot(all(rexp(d, 1) >= 0)) ## dummy
    ## sample killing time
    killing_time <- rexp(1L, rate_killing)
    out[k, ] <- killing_time
  }

  out
}
