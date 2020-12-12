# #### Assertion error messages #####
#

# nolint start
ERR_X_NOT_Y <- "%1$s not %2$s"
ERR_X_NOT_Y_FOR_Z <- paste(ERR_X_NOT_Y, "for %3$s")
# nolint end



# #### Miscellaneous custom assertions ####
#

#' Custom assertions for scalar values
#'
#' @inheritParams assertthat::is.scalar
#'
#' @examples
#' assertthat::see_if(is_positive_number(-1))       ## FALSE
#' assertthat::see_if(is_positive_number(0))        ## FALSE
#' assertthat::see_if(is_positive_number(c(1, 2)))  ## FALSE
#' assertthat::see_if(is_positive_number("1"))      ## FALSE
#' assertthat::see_if(is_positive_number(1))        ## TRUE
#' assertthat::see_if(is_positive_number(pi))       ## TRUE
#'
#' @seealso \code{\link[assertthat:is.scalar]{is.number}}
#'
#' @family assertions
#'
#' @importFrom assertthat is.number
#'
#' @keywords internal
#' @noRd
is_positive_number <- function(x) {
  is.number(x) &&  (x>0)
}

#' @importFrom assertthat on_failure<-
#' @keywords internal
#' @noRd
on_failure(is_positive_number) <- function(call, env) {
  sprintf(ERR_X_NOT_Y, deparse(call$x), "positive number")
}


#' @rdname is_positive_number
#'
#' @examples
#' assertthat::see_if(is_nonnegative_number(-1))      ## FALSE
#' assertthat::see_if(is_nonnegative_number(c(1, 2))) ## FALSE
#' assertthat::see_if(is_nonnegative_number("1"))     ## FALSE
#' assertthat::see_if(is_nonnegative_number(0))       ## TRUE
#' assertthat::see_if(is_nonnegative_number(1))       ## TRUE
#' assertthat::see_if(is_nonnegative_number(pi))      ## TRUE
#'
#' @family assertions
#'
#' @importFrom assertthat is.number
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
  sprintf(ERR_X_NOT_Y, deparse(call$x), "non-negative number")
}


#' Custom assertions for numeric vectors
#'
#' @examples
#' assertthat:see_if(is_nonnegative_vector(1))              ## TRUE
#' assertthat:see_if(is_nonnegative_vector(c(1, 0)))        ## TRUE
#' assertthat:see_if(is_nonnegative_vector(c(1, -1)))       ## FALSE
#'
#' @family assertions
#'
#' @keywords internal
#' @noRd
is_nonnegative_vector <- function(x) {
  is.numeric(x) && all(x >= 0)
}

#' @importFrom assertthat on_failure<-
#' @keywords internal
#' @noRd
on_failure(is_nonnegative_vector) <- function(call, env) {
  sprintf(ERR_X_NOT_Y, deparse(call$x), "non-negative vector")
}

#' @rdname is_nonnegative_vector
#'
#' @examples
#' assertthat:see_if(is_nonzero_vector(1))              ## TRUE
#' assertthat:see_if(is_nonzero_vector(c(1, 0)))        ## TRUE
#' assertthat:see_if(is_nonzero_vector(c(1, -1)))       ## TRUE
#' assertthat:see_if(is_nonzero_vector(0))              ## FALSE
#' assertthat:see_if(is_nonzero_vector(c(0, 0)))        ## TRUE
#'
#' @family assertions
#'
#' @keywords internal
#' @noRd
is_nonzero_vector <- function(x) {
  is.numeric(x) && any(x != 0)
}

#' @importFrom assertthat on_failure<-
#' @keywords internal
#' @noRd
on_failure(is_nonzero_vector) <- function(call, env) {
  sprintf(ERR_X_NOT_Y, deparse(call$x), "non-zero vector")
}


#' Assert if a vector has a certain length
#'
#' @examples
#' assertthat::see_if(has_length(rep(1, 5), 5))      ## TRUE
#' assertthat::see_if(has_length(c(1, 2, 3), 5))     ## FALSE
#'
#' @family assertions
#'
#' @keywords internal
#' @noRd
has_length <- function(x, n) {
  is.vector(x) && (length(x) == n)
}

#' @importFrom assertthat on_failure<-
#' @keywords internal
#' @noRd
on_failure(has_length) <- function(call, env) {
  sprintf(ERR_X_NOT_Y, deparse(call$x), paste("of length", deparse(call$n)))
}

`%has_length%` <- has_length
