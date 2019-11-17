## #### Assertation error messages #####
##
# nolint start
ERR_MARGINRATE_NOT_POS = "%s does not have positive marginal rates"
ERR_NOT_SCALAR_X_NUMBER = "%s is not %s number"
ERR_NOT_RJUMP_NAME = "%s is not allowed name for cpp jump distribution"
ERR_NOT_RJUMP_ARGS = "%s is not valid arglist for cpp jump distribution for %s"
# nolint end


## #### Miscellaneous custom assertations ####
##

#' Custom assertions for scalar values
#'
#' Miscellaneous implementations of assertions for scalar values,
#' e.g. to assert that a value is a positive or non-negative number.
#'
#' @inheritParams assertthat::is.scalar
#'
#' @return `is_positive_number` returns `TRUE` if `x` is a strictly positive,
#'   scalar number and `FALSE` otherwise.
#'
#' @examples
#' assertthat::see_if(is_positive_number(-1)) # FALSE
#' assertthat::see_if(is_positive_number(0)) # FALSE
#' assertthat::see_if(is_positive_number(c(1, 2))) # FALSE
#' assertthat::see_if(is_positive_number("1")) # FALSE
#' assertthat::see_if(is_positive_number(1)) # TRUE
#' assertthat::see_if(is_positive_number(pi)) # TRUE
#'
#' @seealso \code{\link[assertthat:is.scalar]{is.number}}
#'
#' @family assertions
#'
#' @importFrom assertthat assert_that is.number
#'
#' @keywords internal
#' @noRd
is_positive_number <- function(x) {
  is.number(x) &&  x>0
}

#' @importFrom assertthat on_failure<-
#' @keywords internal
#' @noRd
on_failure(is_positive_number) <- function(call, env) {
  sprintf(ERR_NOT_SCALAR_X_NUMBER, deparse(call$x), "positive")
}

#' @rdname is_positive_number
#'
#' @return `is_nonnegative_number` returns `TRUE` if `x` is a strictly non-negative,
#'   scalar number and `FALSE` otherwise.
#'
#' @examples
#' assertthat::see_if(is_nonnegative_number(-1)) # FALSE
#' assertthat::see_if(is_nonnegative_number(c(1, 2))) # FALSE
#' assertthat::see_if(is_nonnegative_number("1")) # FALSE
#' assertthat::see_if(is_nonnegative_number(0)) # TRUE
#' assertthat::see_if(is_nonnegative_number(1)) # TRUE
#' assertthat::see_if(is_nonnegative_number(pi)) # TRUE
#'
#' @family assertions
#'
#' @importFrom assertthat assert_that is.number
#'
#' @keywords internal
#' @noRd
is_nonnegative_number <- function(x) {
  is.number(x) &&  x>=0
}

#' @importFrom assertthat on_failure<-
#' @keywords internal
#' @noRd
on_failure(is_nonnegative_number) <- function(call, env) {
  sprintf(ERR_NOT_SCALAR_X_NUMBER, deparse(call$x), "non-negative")
}


## #### Assertations for MO params ####
##

#' Assertion for the  `intensities` parameter
#'
#' Assert if `intensities` is a valid Marshall-Olkin  parameter.
#' For this, the provided numeric vector must non-negative
#' and fulfil the property
#' \deqn{
#'   \sum_{I \ni i} \lambda_i > 0 \forall i \in \{ 1, \ldots, d\} .
#' }
#'
#' @param intensities A numeric vector intended to be the shock rates of
#'   a Marshall-Olkin distribution.
#'
#' @return TRUE/FALSE
#'
#' @importFrom assertthat assert_that on_failure<-
#' @include sets.R
#' @keywords internal
#' @noRd
is_mo_parameter <- function(intensities) {
  assert_that(is.numeric(intensities), length(intensities) >= 1, all(intensities >= 0))
  assert_that(log2(length(intensities)+1) %% 1 == 0)
  d <- log2(length(intensities)+1)
  marginal_intensities <- numeric(d)
  for (i in 1:d) {
    for (j in 1:(2^d-1)) {
      if (is_within(i, j)) {
        marginal_intensities[i] <- marginal_intensities[[i]] + intensities[[j]]
      }
    }
  }

  all(marginal_intensities > 0)
}
on_failure(is_mo_parameter) <- function(call, env) {
  sprintf(ERR_MARGINRATE_NOT_POS, deparse(call$intensities))
}


## #### Assertations for exMO params ####
##

#' Assertion for the `ex_intensities` parameter
#'
#' Assert if `ex_intensities` is a valid exchangeable Marshall-Olkin parameter.
#' For this, the numeric vector must be non-negative and fulfil the property
#' \deqn{
#'   \sum_{j=0}^{d-1} \choose{d-1}{j} \lambda_{j+1} > 0 .
#' }
#'
#' @param ex_intensity A numeric vector intended to be the shock rates of
#'   a Marshall-Olkin distribution.
#'
#' @return TRUE/FALSE
#'
#' @importFrom assertthat assert_that
#' @keywords internal
#' @noRd
is_exmo_parameter <- function(ex_intensities) {
  assert_that(is.numeric(ex_intensities), length(ex_intensities) >= 1, all(ex_intensities >= 0))
  d <- length(ex_intensities)
  marginal_intensity <- sum(vapply(0:(d-1), function(y) choose(d-1, y) * ex_intensities[[y+1]], FUN.VALUE=0.5)) # nolint

  marginal_intensity > 0
}
on_failure(is_exmo_parameter) <- function(call, env) {
  sprintf(ERR_MARGINRATE_NOT_POS, deparse(call$ex_intensities))
}


## #### Assertations for CPP jump distr. params ####
##

#' Assertation for jump distribution and params
#'
#' @param rjump_name name of sampling function for jump distribution
#'
#' @return `is_rjump_name` returns `TRUE` if `rjump_name` is a `string`
#'   and is contained in list of allowed distributions and `FALSE` otherwise.
#'
#' @examples
#' assertthat::see_if(is_rjump_name("rnorm")) # FALSE
#' assertthat::see_if(is_rjump_name("rexp")) # TRUE
#'
#' @family assertions
#'
#' @importFrom assertthat assert_that is.string
#' @keywords internal
#' @noRd
is_rjump_name <- function(rjump_name) {
  assert_that(is.string(rjump_name))
  allowed_names <- c("rexp", "rposval")

  rjump_name %in% allowed_names
}

#‘ @importFrom assertthat on_failure<-
#' @keywords internal
#' @noRd
on_failure(is_rjump_name) <- function(call, env) {
  sprintf(ERR_NOT_RJUMP_NAME, deparse(call$rjump_name))
}


#' @rdname is_rjump_name
#'
#' @param rjump_arg_list argument list for `rjump_name`
#'
#' @return `is_rjump_arg_list` returns `TRUE` if a call to `do.call` with
#'   `rjump_name` and `args=c("n" = 1, rjump_arg_list)` is successful and
#'   `FALSE` otherwise.
#'
#' @examples
#' assertthat::see_if(is_rjump_arg_list("rexp", list())) # FALSE
#' assertthat::see_if(is_rjump_arg_list("rexp", list("rate"=0.5))) # TRUE
#'
#' @family assertions
#'
#' @importFrom assertthat assert_that is.error %has_args%
#'
#' @keywords internal
#' @noRd
is_rjump_arg_list <- function(rjump_name, rjump_arg_list) {
  assert_that(is_rjump_name(rjump_name), is.list(rjump_arg_list))
  if (!get(rjump_name) %has_args% names(rjump_arg_list)) {
    return(FALSE)
  }

  suppressWarnings(x <- try(do.call(rjump_name, args=c("n"=1, rjump_arg_list)), silent=TRUE))
  !is.error(x) && !is.na(x) && is_nonnegative_number(x)
}

#‘ @importFrom assertthat on_failure<-
#' @keywords internal
#' @noRd
on_failure(is_rjump_arg_list) <- function(call, env) {
  sprintf(ERR_NOT_RJUMP_ARGS, deparse(call$rjump_arg_list), deparse(call$rjump_name))
}
