#include "sets.h"

//' @keywords internal
//' @noRd
// [[Rcpp::export]]
bool Rcpp__is_within(const unsigned int &i, const unsigned int &j) {
  return is_within(i, j);
} // bool Rcpp__is_within(const unsigned int &i, const unsigned int &j);
