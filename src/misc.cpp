#include <Rcpp.h>
#include <rmo.hpp>

using namespace Rcpp;

//' @keywords internal helper
//' @noRd
// [[Rcpp::export]]
bool Rcpp__is_within(const R_xlen_t i, const R_xlen_t j) {
  using mo::math::is_within;
  return is_within(static_cast<std::size_t>(i - 1),
                   static_cast<std::size_t>(j - 1));
}
