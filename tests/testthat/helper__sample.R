## File contains test functions for various simulation Marshall--Olkin
## simulation algorithms.
##
## Naming convention for the functions: test__rmo_<name>_<specialisation>_R
##
## #### Setup ####
##
if (!"assertthat" %in% .packages()) {
  library("assertthat", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
}

require_R_version <- function( # nolint
    version_string = paste0(major, minor, sep="."),
    major = R.version$major,
    minor = R.version$minor) {
  R_version_string <- paste(R.version$major, R.version$minor, sep=".") # nolint
  return(1 == compareVersion(R_version_string, version_string))
}

## Use pre R 3.6.x sample RNG since the new one is not yet
## implemented in Rcpp.
if (require_R_version("3.6.0")) {
  suppressWarnings(
    RNGkind(
      kind="default",
      normal.kind = "default",
      sample.kind="Rounding"))
} else {
  suppressWarnings(
    RNGkind(
      kind="default",
      normal.kind = "default"))
}
