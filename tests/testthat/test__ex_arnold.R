context("Exchangeable Arnold model")
use_seed <- 1632L

## Test that the implementation of the modified version of the
## Arnold model for exchangeable distributions works as intended
## for d = 2.
test_that("Exchangeable Arnold model for d = 2", {
  n <- 100L

  args <- list("d" = 2L, ex_intensities = c(0.5, 1))
  expect_equal_sampling_result("rmo_ex_arnold", "test__rmo_ex_arnold_bivariate_R",
                               args, n, use_seed)
})



## Test that alternativ eimplementation based on the `a`
## parameters is equivalent for d = 5.
test_that("Alternative implementation", {
  n <- 100L

  args <- list("d" = 5L, ex_intensities = c(0.4, 0.3, 0.2, 0.2, 0.1))
  expect_equal_sampling_result("rmo_ex_arnold", "test__rmo_ex_arnold_alternative_R",
                               args, n, use_seed)
})
