context("Arnold model")

## Test that the implementation Arnold model
## works as expected for d = 2.
test_that("Arnold model implementation for d = 2", {
  n <- 100
  intensities <- c(0.5, 0.4, 0.2)

  set.seed(1632)
  x <- rmo_arnold(n, 2, intensities)

  set.seed(1632)
  total_intensity <- sum(intensities)
  transition_probabilities <-intensities / total_intensity
  out <- matrix(0, nrow = n, ncol = 2)
  for (i in 1:n) {
    destroyed <- rep(FALSE, 2)
    while (!all(destroyed)) {
      W <- rexp(1, total_intensity) # nolint
      Y <- sample.int(3, 1, replace=FALSE, prob = transition_probabilities) # nolint
      out[i, !destroyed] <- out[i, !destroyed] + W
      if (Y == 1) {
        destroyed[1] <- TRUE
      } else if (Y == 2) {
        destroyed[2] <- TRUE
      } else {
        destroyed <- rep(TRUE, 2)
      }
    }
  }

  expect_equal(x, out)
})
