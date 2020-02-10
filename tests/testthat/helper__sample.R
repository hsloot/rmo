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
## Use pre R 3.6.x sample RNG since the new one is not yet
## implemented in Rcpp.
suppressWarnings(RNGkind(kind="Mersenne-Twister", normal.kind = "Inversion",
                         sample.kind="Rounding"))
