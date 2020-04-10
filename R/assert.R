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
#' Miscellaneous implementations of assertions for scalar values,
#' e.g. to assert that a value is a positive or a non-negative number.
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
  is.number(x) &&  x>0
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



# #### Assertion for dimension parameters ####
#

#' Assertions dimension parameters
#'
#' @inheritParams assertthat::is.scalar
#'
#' @examples
#' assertthat::see_if(is_dimension(-1L))  ## FALSE
#' assertthat::see_if(is_dimension("2"))  ## FALSE
#' assertthat::see_if(is_dimension(2L))   ## TRUE
#' assertthat::see_if(is_dimension(15))   ## TRUE
#' assertthat::see_if(is_dimension(31L))  ## TRUE
#' assertthat::see_if(is_dimension(32L))  ## TRUE
#'
#' @family assertions
#'
#' @importFrom assertthat is.count
#'
#' @keywords internal
#' @noRd
is_dimension <- function(x) {
  is.count(x) && x>1L
}

#' @importFrom assertthat on_failure<-
#' @keywords internal
#' @noRd
on_failure(is_dimension) <- function(call, env) {
  sprintf(ERR_X_NOT_Y, deparse(call$x), "dimension parameter")
}


#' @rdname is_dimension
#'
#' @details Since `R` can only represent 32bit `integers` properly
#' (higher integers are internally represented as `double`), we get
#' problems passing `intensities` vectors to `Rcpp` for classical
#' Marshall-Olkin models in \eqn{d>31}.
#'
#' @examples
#' assertthat::see_if(is_dimension(-1L))  ## FALSE
#' assertthat::see_if(is_dimension("2"))  ## FALSE
#' assertthat::see_if(is_dimension(2L))   ## TRUE
#' assertthat::see_if(is_dimension(15))   ## TRUE
#' assertthat::see_if(is_dimension(31L))  ## TRUE
#' assertthat::see_if(is_dimension(32L))  ## FALSE
#'
#' @family assertions
#'
#' @keywords internal
#' @noRd
is_32bit_compliant_dimension <- function(x) {
  is_dimension(x) && x<32L
}

#' @importFrom assertthat on_failure<-
#' @keywords internal
#' @noRd
on_failure(is_32bit_compliant_dimension) <- function(call, env) {
  sprintf(ERR_X_NOT_Y, deparse(call$x), "32bit compliant dimension parameter")
}



# #### Assertions for MO params ####
#

#' Assertion for MO parameter
#'
#' @details
#' `is_mo_parameter` asserts if `d` and `intensities` are a
#' valid Marshall-Olkin parameterisation.
#' - `d` must be a 32bit compliant dimension
#' - `intensities` must be a non-negeative, numeric vector with
#' length equal to \eqn{2^d-1}
#' - The implied marginal rates from `intensities` must be all
#' strictly positive, i.e.
#' \deqn{
#'   \sum_{I \ni i} \lambda_i > 0 \forall i \in \{ 1, \ldots, d\} .
#' }
#'
#' @importFrom assertthat assert_that on_failure<-
#' @include RcppExports.R
#'
#' @keywords internal
#' @noRd
is_mo_parameter <- function(d, intensities) {
  assert_that(is_32bit_compliant_dimension(d))
  if (!is.numeric(intensities) || !(length(intensities) == 2^d-1) ||
      any(intensities<0))
    return(FALSE)

  marginal_intensities <- numeric(d)
  for (i in 1:d) {
    for (j in 1:(2^d-1)) {
      if (Rcpp__is_within(i, j)) {
        marginal_intensities[i] <- marginal_intensities[[i]] +
          intensities[[j]]
      }
    }
  }
  all(marginal_intensities > 0)
}

#' @importFrom assertthat on_failure<-
#' @keywords internal
#' @noRd
on_failure(is_mo_parameter) <- function(call, env) {
  sprintf(ERR_X_NOT_Y_FOR_Z, deparse(call$intensities),
          "MO parameter", paste("dimension", deparse(call$d)))
}


#' Assertion for exMO parameter
#'
#' @details
#' `is_ex_mo_parameter` asserts if `d` and `ex_intensities` are a valid
#' parametrisation of the exchangeable Marshall-Olkin distribution, i.e.
#' - `d` is a valid dimension
#' - `ex_intensities` is a non-negative, numeric vector of length equal to `d`
#' - at least one entry of `ex_intensities` is strictly positive
#'
#' @importFrom assertthat assert_that
#' @keywords internal
#' @noRd
is_ex_mo_parameter <- function(d, ex_intensities) {
  assert_that(is_dimension(d))
  (is.numeric(ex_intensities) && (length(ex_intensities) == d) &&
    all(ex_intensities>=0) && any(ex_intensities>0))
}

#' @importFrom assertthat on_failure<-
#' @keywords internal
#' @noRd
on_failure(is_ex_mo_parameter) <- function(call, env) {
  sprintf(ERR_X_NOT_Y_FOR_Z, deparse(call$ex_intensities),
          "exMO parameter", paste("dimension", deparse(call$d)))
}


#' Assertion for the Lévy-fraily MO parameter
#'
#' @importFrom assertthat assert_that
#'
#' @keywords internal
#' @noRd
is_lfm_cpp_mo_parameter <- function(d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list) {
  assert_that(is_dimension(d))
  assert_that(is_nonnegative_number(rate), is_nonnegative_number(rate_killing),
    is_nonnegative_number(rate_drift),
    is_positive_number(rate + rate_killing + rate_drift),
    is_rjump_parameter(rjump_name, rjump_arg_list))
  TRUE
}


#' Assertion for the multivariate Cuadras-Augé parameter
#'
#' @importFrom assertthat assert_that
#' @keywords internal
#' @noRd
is_cuadras_auge_parameter <- function(d, alpha, beta) {
  assert_that(is_dimension(d), is_nonnegative_number(alpha),
    is_nonnegative_number(beta), is_positive_number(alpha + beta))
  TRUE
}



# #### Assertions for CPP jump distribution params ####
#

#' Assertion for jump distribution name
#'
#' @details
#' As of now, we only allow the exponential distribution
#'   or fixed jump sizes.
#'
#' @examples
#' assertthat::see_if(is_rjump_name("rnorm")) ## FALSE
#' assertthat::see_if(is_rjump_name("rexp"))  ## TRUE
#'
#' @family assertions
#'
#' @importFrom assertthat assert_that is.string
#'
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
  sprintf(ERR_X_NOT_Y, deparse(call$rjump_name),
    "allowed cpp jump distribution")
}


#' Assertion for jump distribution parameter
#'
#' @details
#' Performs a test run with the given name and parameter list and
#'  throws an error if this is unsuccessful. Warning: Modifies RNG state.
#'
#' @examples
#' assertthat::see_if(is_rjump_parameter("rexp", list()))            ## FALSE
#' assertthat::see_if(is_rjump_parameter("rexp", list("rate"=0.5)))  ## TRUE
#'
#' @family assertions
#'
#' @importFrom assertthat assert_that is.error %has_args%
#'
#' @keywords internal
#' @noRd
is_rjump_parameter <- function(rjump_name, rjump_arg_list) {
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
on_failure(is_rjump_parameter) <- function(call, env) {
  sprintf(ERR_X_NOT_Y_FOR_Z, deparse(call$rjump_arg_list),
    "argument list", deparse(call$rjump_name))
}
