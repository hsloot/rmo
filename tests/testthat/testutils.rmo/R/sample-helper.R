#' @seealso \url{https://hsloot.github.io/rmo/articles/The-binary-representation.html}
#' @noRd
#' @export
is_within <- function(i, j) { # nolint
  count <- 1
  while (j > 0) {
    if (1 == (j %% 2) && count == i) ## <=> c_{i-1} = 1
      return(TRUE)
    j <- j %/% 2
    count <- count + 1
  }

  return(FALSE)
}

#' @noRd
#' @export
last <- function(x) {
  x[[length(x)]]
}

#' @noRd
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

#' @noRd
#' @export
rposval <- function(n, value = 1) {
  rep(value, times=n)
}

#' @noRd
#' @importFrom stats runif
#' @export
rpareto <- function(n, alpha = 1, x0 = 1e-4) {
  x0 / (stats::runif(n)) ^ (1/alpha)
}
