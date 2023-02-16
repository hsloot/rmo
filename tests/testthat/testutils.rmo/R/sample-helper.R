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

#' Test if component `i` is in set `j`
#'
#' @param i Component
#' @param j Set
#'
#' @details
#' The function returns `TRUE`, if the i's bit of j is set (with base 1)
#' @seealso
#'   \url{https://hsloot.github.io/rmo/articles/The-binary-representation.html}
#' @examples
#' is_within(1, 1)
#' is_within(1, 2)
#' is_within(2, 2)
#' is_within(1, 3)
#' @export
is_within <- function(i, j) { # nolint
    count <- 1
    while (j > 0) {
        if (1 == (j %% 2) && count == i) { ## <=> c_{i-1} = 1
            return(TRUE)
        }
        j <- j %/% 2
        count <- count + 1
    }

    return(FALSE)
}

#' Last value in a vector or list
#'
#' @param x Vector or list
#'
#' @examples
#' last(rep(1, times = 5))
#' @export
last <- function(x) {
    x[[length(x)]]
}

#' Sample from the Exponential distribution
#'
#' @param n Number of samples
#' @param rate Rate of the Exponential distribution
#'
#' @details
#' Wraps [stats::rexp()] and returns zeros if `rate == Inf` and Inf-values if
#' `rate == 0`.
#'
#' @examples
#' rexp(10, rate = 1)
#' rexp(10, rate = 0.5)
#' stopifnot(all(rexp(10, rate = Inf) == 0))
#' stopifnot(all(rexp(10, rate = 0) == Inf))
#' @importFrom stats rexp
#' @export
rexp <- function(n, rate = 1) {
    if (0 == rate) {
        return(rep(Inf, n))
    } else if (Inf == rate) {
        return(rep(0, n))
    }

    stats::rexp(n, rate)
}

#' Sample a deterministic value
#'
#' @param n Number of samples
#' @param value The value that is supposed to be sampled
#'
#' @examples
#' rposval(10, value = 0.5)
#' @export
rposval <- function(n, value = 1) {
    rep(value, times = n)
}

#' Sample from the Pareto distribution
#'
#' @param n Number of samples
#' @param alpha Scale parameter of the Pareto distribution
#' @param x0 Lower bound parameter of the Pareto distribution
#'
#' @examples
#' rpareto(10, alpha = 1, x0 = 1e-4)
#' @importFrom stats runif
#' @export
rpareto <- function(n, alpha = 1, x0 = 1e-4) {
    x0 / (stats::runif(n))^(1 / alpha)
}
