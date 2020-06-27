#include <mo.hpp>

//' @keywords internal
//' @noRd
// [[Rcpp::export]]
bool Rcpp__is_within(const R_xlen_t i, const R_xlen_t j) {
  return mo::math::is_within(i-1, j-1);
}
