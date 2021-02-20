# # MIT License
#
# Copyright (c) 2021 Henrik Sloot
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

## #### Lévy frailty model ####

#' Alternative implementation of com.-case via LFM
#'
#' @param n Number of samples (> 0)
#' @param d Dimension (> 0)
#' @param rate Intensity of compound Poisson subordinator (== 0)
#' @param rate_killing Intensity of the killing-time (> 0)
#' @param rate_drift Drift of the compound Poisson subordinator (== 0)
#' @param rjump_name Name of the sampling function for the jumps
#'   of the compound Poisson subordinator
#' @param rjump_arg_list List with parameters for the sampling function for
#'   the jumps of the compound Poisson subordinator
#'
#' @examples
#' rextmo_lfm_comonotone(10, 3, 0, 1, 0, "rposval", list("value"=1))
#' @include sample-helper.R
#' @export
rextmo_lfm_comonotone <- function( # nolint
    n, d = 2,
    rate = 0, rate_killing = 1, rate_drift = 0,
    rjump_name = "rposval", rjump_arg_list = list("value" = 0)) {
  stopifnot(
    is.numeric(n) && 1L == length(n) && 0 == n %% 1 && n > 0 &&
    is.numeric(d) && 1L == length(d) && 0 == d %% 1 && d > 0 &&
    is.numeric(rate) && 1L == length(rate) && rate == 0 &&
    is.numeric(rate_killing) && 1L == length(rate_killing) &&
      rate_killing > 0 &&
    is.numeric(rate_drift) && 1L == length(rate_drift) && rate_drift == 0 &&
    is.character(rjump_name) && 1L == length(rjump_name) &&
      rjump_name %in% c("rexp", "rposval", "rpareto"))

  out <- matrix(nrow = n, ncol = d)
  for (k in 1:n) {
    ## we do not need it here, but we have to sample the unit exponential
    ## barriers to keep the random number generators in sync
    barrier_values <- rexp(d, rate = 1) # nolint
    ## sample killing time
    killing_time <- rexp(1L, rate = rate_killing)
    out[k, ] <- killing_time
  }

  out
}
