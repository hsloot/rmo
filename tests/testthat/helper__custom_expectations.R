## This file contains custom expectations to be used in
## unit tests.
##

format_args <- function(args, ...) {
  paste(paste0(" * ", names(args), " = \n\t", format(args, ...)), collapse = "\n")
}

## #### Compare simulation results ####
##

#' Compare the output of two sampling algorithms
#'
#' Implementation of a custom expectation to compare two sampling algorithms
#' for a given number of samples and arguments.
#'
#' @param object Name of the first sampling algorithm
#' @param expected Name of the second sampling algorithm
#' @param arguments Parameters for both algorithms (expected to be the same)
#' @param n Number of samples
#' @param use_seed The seed which is set before each call to sampling algorithm
#'
#' @seealso \code{\link[testthat]{expect_equal}}
#' @seealso \url{https://testthat.r-lib.org/articles/custom-expectation.html}
expect_equal_sampling_result <- function(object, expected, arguments, n = 100L,
    use_seed = 1623L, info = NULL, label = NULL,
    expected.label = NULL, ...) {
  act <- quasi_label(rlang::enquo(object), label, arg = "object")
  exp <- quasi_label(rlang::enquo(expected), expected.label, arg = "expected")
  args <- quasi_label(rlang::enquo(arguments), "Arguments", arg = "arguments")

  assert_that(is.string(act$val), is.string(exp$val), is.count(n),
              is.count(use_seed))

  set.seed(use_seed)
  x <- do.call(act$val, args = c("n" = n, args$val))
  set.seed(use_seed)
  y <- do.call(exp$val, args = c("n" = n, args$val))

  comp <- compare(x, y, ...)
  expect(
    comp$equal,
    sprintf(paste0(
      "Sample results of %s not equal to those of %s.",
      "\nSeed: %s",
      "\nNumber of simulations: %s",
      "\n%s:",
      "\n%s",
      "\n\n%s"
      ),act$lab, exp$lab, use_seed, n, args$lab,
      format_args(args$val, justify = "right", digits = 2L), comp$message),
    info = info
  )

  invisible(act$val)
}
