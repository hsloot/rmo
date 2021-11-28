# #### Hierarchical intensities ####

#' @keywords internal
#' @noRd
intensities_hierarchical <- function(d1, d2, lambda, eta, a, alpha) { # nolint
  ex_intensities_1 <- rmo::ex_intensities_gamma(d1, a)
  ex_intensities_2 <- rmo::ex_intensities_alpha_stable(d2, alpha)

  intensities <- lambda * rmo::intensities_poisson(d1+d2, eta)
  for (j in seq_along(intensities)) {
    count_1 <- 0
    count_2 <- 0
    for (i in 1:d1) {
      count_1 <- count_1 + rmo:::Rcpp__is_within(i, j)
    }
    for (i in 1:d2) {
      count_2 <- count_2 + rmo:::Rcpp__is_within(d1+i, j)
    }

    if (count_1 > 0 && count_2 == 0) {
      intensities[j] <- intensities[j] + ex_intensities_1[count_1]
    } else if (count_2 > 0 && count_1 == 0) {
      intensities[j] <- intensities[j] + ex_intensities_2[count_2]
    }
  }

  intensities
}
