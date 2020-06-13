## This file contains custom expectations to be used in
## unit tests.
##
if (!"assertthat" %in% .packages()) {
  library("assertthat", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
}
if (!"rlang" %in% .packages()) {
  library("rlang", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
}


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
expect_equal_sampling_result <- function(object, expected, arguments, n,
    use_seed = 1623L, env = parent.frame(), ...) {
  act <- testthat::quasi_label(rlang::enquo(object), NULL, arg = "object")
  exp <- testthat::quasi_label(rlang::enquo(expected), NULL, arg = "expected")
  args <- testthat::quasi_label(rlang::enquo(arguments), "Arguments", arg = "arguments")

  assertthat::assert_that(assertthat::is.string(act$val), assertthat::is.string(exp$val),
    missing(n) || assertthat::is.count(n), assertthat::is.count(use_seed))

  if (!missing(n)) {
    arg_list <- c("n" = n, args$val)
  } else {
    arg_list <- args$val
  }
  set.seed(use_seed)
  x <- do.call(act$val, args = arg_list, envir = env)
  set.seed(use_seed)
  y <- do.call(exp$val, args = arg_list, envir = env)

  comp <- testthat::compare(x, y, ...)
  testthat::expect(
    comp$equal,
    sprintf(paste0(
      "Sample results of %s not equal to those of %s.",
      "\nSeed: %s",
      "\nNumber of simulations: %s",
      "\n%s:",
      "\n%s",
      "\n\n%s"
    ), act$lab, exp$lab, use_seed, ifelse(!missing(n), n, 1L), args$lab,
      format_args(args$val, justify = "right", digits = 2L), comp$message),
    info = NULL
  )

  invisible(act$val)
}
