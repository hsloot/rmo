#' @keywords internal
#' @noRd
test__is_within_R <- function(i, j) { # nolint
  count <- 1
  while (j > 0) {
    if (1 == (j %% 2) && count == i)
      return(TRUE)
    j <- j %/% 2
    count <- count + 1
  }

  return(FALSE)
}
