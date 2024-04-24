value_of_naive <- function(f, x, difference_order = 0L, n = 1L, k = 0L, cscale = 1, ...) { # nolint
  if (isTRUE(0L == difference_order)) {
    out <- f(cscale * x, ...)
  } else {
    out <- (-1)^(difference_order - 1L) *
      sapply(
        x,
        function(.x) {
          diff(
            f(cscale * (.x + (0:difference_order)), ...),
            differences = difference_order
          )
        }
      )
  }

  choose(n, k) * out
}

calc_ex_shock_size_arrival_intensities_naive <- function(f, d, cscale = 1, ...) { # nolint
  sapply(
    seq_len(d),
    function(i) {
      value_of_naive(
        f,
        x = d - i,
        difference_order = i,
        n = d, k = i,
        cscale = cscale,
        ...
      )
    }
  )
}

ex_qmatrix_naive <- function(f, d, cscale = 1, ...) {
  outer(
    as.integer(c(0, seq_len(d))), as.integer(c(0, seq_len(d))),
    Vectorize(
      function(i, j) {
        if (j > i) {
          value_of_naive(
            f,
            x = d - j,
            difference_order = j - i,
            n = d - i, k = j - i,
            cscale = cscale,
            ...
          )
        } else if (j == i) {
          -f(cscale * (d - i), ...)
        } else {
          0
        }
      }
    )
  )
}
