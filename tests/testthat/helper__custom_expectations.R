#' `expect*` for two random number generation functions
#'
#' Implementation of a custom expectation which can be used with
#' `testthat` to write consistent code to test functions for random
#' number generation.
#'
#' @param rn_generator Name/function of the random number generation method
#' @param rn_generator_test Name/function of the corresponding test
#'   implementation
#' @param arg_list Argument list for both methods
#' @param n Number of samples
#' @param use_seed The seed (which is reset before each callto an RNG method)
#' @param RNG_kind_arg_list A list with arguments for `RNGkind`
#' @param set_seed A function to reset the seed
#' @param RNG_kind A function to choose the underlying RNG
#' @param \\dots Further parameters for `[testthat::compare()]`
#'
#' The function appends `arg_list` with `c("n" = n)` if `n` is provided,
#' initialises the RNG with `rlang::exec(RNG_kind, !!!RNG_kind_arg_list)`
#' (this can be avoided by setting `RNG_kind_arg_list = NULL`), and
#' calls `rn_generator` and `rn_generator_test` with provided arguments via
#' `[rlang::exec()]`.
#' Each call is preceeded by a call to `set_seed(use_seed)` to ensure
#' reproducibility.
#'
#' @examples
#' rexp_test <- function(n, rate) {
#'  sapply(1:n, function(x) stats::rexp(1L, rate))
#' }
#' testthat::test_that("rexp equal to rexp_test?", {
#'  expect_equal_rn_generation(
#'    stats::rexp, rexp_test,
#'    list("rate" = 2), 10L, use_seed=1623L)
#' })
#'
#' @seealso \code{\link[testthat]{expect_equal}}
#' @seealso \url{https://testthat.r-lib.org/articles/custom-expectation.html}
expect_equal_rn_generation <- function(
    rn_generator,
    rn_generator_test,
    arg_list,
    n,
    use_seed,
    RNG_kind_arg_list = RNG_kind_arg_list_default(), # nolint
    set_seed = base::set.seed,
    RNG_kind = base::RNGkind, # nolint
    env = parent.frame(), ...) {
  ## Capture rn_generator, rn_generator_test, and arguments with labels
  rn_generator <- testthat::quasi_label(
    rlang::enquo(rn_generator), NULL, arg = "rn_generator")
  rn_generator_test <- testthat::quasi_label(
    rlang::enquo(rn_generator_test), NULL, arg = "rn_generator_test")
  arg_list <- testthat::quasi_label(
    rlang::enquo(arg_list), "Arguments", arg = "arg_list")

  ## Conventional checks to catch user errors early on
  assertthat::assert_that(
    assertthat::is.string(rn_generator$val) ||
      rlang::is_function(rn_generator$val),
    assertthat::is.string(rn_generator_test$val) ||
      rlang::is_function(rn_generator_test$val),
    missing(n) ||
      assertthat::is.count(n),
    assertthat::is.count(use_seed))

  ## Set up call arguments
  if (!missing(n)) {
    arg_list$val <- c("n" = n, arg_list$val)
  }

  ## Set up RNG
  if (!is.null(RNG_kind_arg_list))
    rlang::exec(RNG_kind, !!!RNG_kind_arg_list, .env = env)

  ## Evaluate expressions and compare results
  set_seed(use_seed)
  x <- rlang::exec(rn_generator$val, !!!arg_list$val, .env = env)
  set_seed(use_seed)
  y <- rlang::exec(rn_generator_test$val, !!!arg_list$val, .env = env)

  comp <- testthat::compare(x, y, ...)

  ## Create testthat output
  testthat::expect(
    ok = comp$equal,
    failure_message = sprintf(
      paste0(
        "Random number generation results of %s not equal to those of %s.",
        "\nSeed: %s",
        "\n%s:",
        "\n%s",
        "\n\n%s"
      ),
      rn_generator$lab, rn_generator_test$lab, use_seed,
      arg_list$lab,
      format_args(arg_list$val, justify = "right", digits = 2L),
      comp$message)
  )

  invisible(rn_generator$val)
}

RNG_kind_arg_list_default <- function() {
  out <- list("kind" = "default", "normal.kind" = "default")
  if (require_R_version() >= "3.6.0")
    out["sample.kind"] <- "default"
  out
}

format_args <- function(args, ...) {
  paste(
    paste0(
      " * ", names(args), " = \n\t", format(args, ...)
    ),
    collapse = "\n"
  )
}
