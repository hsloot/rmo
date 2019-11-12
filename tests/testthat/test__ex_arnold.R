context("Exchangeable Arnold model")

## Test that the implementation of the modified version of the
## Arnold model for exchangeable distributions works as intended
## for d = 2.
test_that("Exchangeable Arnold model for d = 2", {
  n <- 100
  ex_intensities <- c(0.5, 1)

  set.seed(1632)
  x <- rmo_ex_arnold(n, 2, ex_intensities)

  set.seed(1632)
  y <- matrix(0, nrow=n, ncol=2)
  total_intensity <- 2*ex_intensities[[1]] + ex_intensities[[2]]
  transition_probabilities <- c(2*ex_intensities[[1]], ex_intensities[[2]]) /
    total_intensity

  for (i in 1:n) {
    epsilon <- rexp(1, total_intensity)
    num_affected <- sample.int(2, 1, replace = FALSE, prob = transition_probabilities)

    if (num_affected < 2) {
      y[i, 1] <- epsilon

      epsilon <- rexp(1, sum(ex_intensities))
      num_affected <- sample.int(1, 1, replace = FALSE) # dummy
      y[i, 2] <- y[i, 1] + epsilon

      perm <- sample.int(2, 2, replace=FALSE)
      y[i, ] <- y[i, perm]
    } else {
      y[i, ] <- epsilon
      perm <- sample.int(2, 2, replace=FALSE) # dummy
    }
  }

  expect_equal(x, y)
})



## Test that alternativ eimplementation based on the `a`
## parameters is equivalent for d = 5.
test_that("Alternative implementation", {
  n <- 100

  ex_intensities <- c(0.4, 0.3, 0.2, 0.2, 0.1)
  ex_a <- vapply(0:(5-1), function(x) sum(vapply(0:(5-x-1), function(y) choose(5-x-1, y) * ex_intensities[[y+1]], FUN.VALUE=0.5)), FUN.VALUE=0.5)

  set.seed(1632)
  x <- rmo_ex_arnold(n, 5, ex_intensities)

  set.seed(1632)
  y <- matrix(0, nrow=n, ncol=5)

  for (i in 1:n) {
    ex_a_tmp <- ex_a
    d_tmp <- 5
    while (d_tmp > 0) {
      ex_a_tmp <- ex_a_tmp[1:d_tmp]
      ex_intensities_tmp <- vapply(1:d_tmp, function(x) sum(vapply(0:(x-1), function(y) (-1)^(y) * choose(x - 1, y) * ex_a_tmp[[d_tmp - x + y + 1]], FUN.VALUE = 0.5)), FUN.VALUE = 0.5) # nolint

      transition_probabilities <- vapply(1:d_tmp, function(x) choose(d_tmp, x), FUN.VALUE = 0.5) *
        ex_intensities_tmp # intermediate result
      total_intensity <- sum(transition_probabilities)
      transition_probabilities <- transition_probabilities / total_intensity

      epsilon <- rexp(1, total_intensity)
      num_affected <- sample.int(d_tmp, 1, replace = TRUE, prob = transition_probabilities)

      y[i, (5-d_tmp+1):5] <- y[i, (5-d_tmp+1):5] + epsilon
      d_tmp <- d_tmp - num_affected
    }

    perm <- sample.int(5, 5, replace = FALSE)
    y[i, ] <- y[i, perm]
  }

  expect_equal(x, y)
})
