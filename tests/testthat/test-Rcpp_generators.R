use_seed <- 1632L
n <- 1e5L

test_that("ExpGenerator works as expected", {
  args <- list("rate" = 0.5)
  expect_equal_rn_generation(
    "rtest__exponential", testutils.rmo::rexp,
    args, n, use_seed
  )
  args <- list("rate" = 2)
  expect_equal_rn_generation(
    "rtest__exponential", testutils.rmo::rexp,
    args, n, use_seed
  )
  args <- list("rate" = 0)
  expect_equal_rn_generation(
    "rtest__exponential", testutils.rmo::rexp,
    args, n, use_seed
  )
  rexp_rate_is_inf <- function(n, rate) {
    stopifnot(rate == Inf)
    rep(0., times = n)
  }
  args <- list("rate" = Inf)
  expect_equal_rn_generation(
    "rtest__exponential", testutils.rmo::rexp,
    args, n, use_seed
  )
})

test_that("ParetoGenerator works as expected", {
  args <- list("alpha" = 0.5, "x0" = 0.1)
  expect_equal_rn_generation(
    "rtest__pareto", testutils.rmo::rpareto,
    args, n, use_seed
  )
  args <- list("alpha" = 0.1, "x0" = 2)
  expect_equal_rn_generation(
    "rtest__pareto", testutils.rmo::rpareto,
    args, n, use_seed
  )
})

test_that("FixedDblGenerator works as expected", {
  args <- list("value" = 1)
  expect_equal_rn_generation(
    "rtest__deterministic", testutils.rmo::rposval,
    args, n, use_seed
  )
})

test_that("CountReplaceGenerator works as expected", {
  sample_count_replace_base_0 <- function(n,
                                          d = length(probabilities),
                                          probabilities = NULL) {
    sapply(
      1:n,
      function(x) {
        sample.int(
          n = d, size = 1,
          prob = probabilities, replace = FALSE
        ) - 1
      }
    )
  }
  probabilities <- c(8, 7, 3, 10, 6, 1, 2, 9, 5, 4)
  args <- list("d" = length(probabilities), "probabilities" = probabilities)
  expect_equal_rn_generation(
    "rtest__discrete", "sample_count_replace_base_0",
    args, n, use_seed
  )

  probabilities <- probabilities / sum(probabilities)
  args <- list("d" = length(probabilities), "probabilities" = probabilities)
  expect_equal_rn_generation(
    "rtest__discrete", "sample_count_replace_base_0",
    args, n, use_seed
  )

  probabilities <- rep(1, 10) / 10
  args <- list("d" = length(probabilities), "probabilities" = probabilities)
  expect_equal_rn_generation(
    "rtest__discrete", "sample_count_replace_base_0",
    args, n, use_seed
  )

  probabilities <- c(10, 20, 0.1)
  args <- list("d" = length(probabilities), "probabilities" = probabilities)
  expect_equal_rn_generation(
    "rtest__discrete", "sample_count_replace_base_0",
    args, n, use_seed
  )

  args <- list("d" = 10, "probabilities" = NULL)
  expect_equal_rn_generation(
    "rtest__discrete", "sample_count_replace_base_0",
    args, n, use_seed
  )
})
