#' Domain error message
#'
#' @param variable
#'     a string with the variable name.
#' @param domain
#'     a string with the domain name.
#'
#' @examples
#' error_msg_domain("a", "N[0,)")
#'
#' @importFrom checkmate assert_string
#'
#' @keywords internal
#' @noRd
error_msg_domain <- function(variable, domain) {
  assert_string(variable)
  assert_string(domain)

  sprintf("Parameter %s must be of type %s", variable, domain)
}
