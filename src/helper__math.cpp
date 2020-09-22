#include <Rcpp.h>
#include <rmo.hpp>

using namespace Rcpp;
using namespace mo::math;

// [[Rcpp::export]]
std::size_t mo_internal__binomial_coefficient(const std::size_t n, const std::size_t k) {
  return binomial_coefficient(n, k);
}

// [[Rcpp::export]]
double mo_internal__binomial_coefficient_factor(const double x, const std::size_t n,
                                         const std::size_t k) {
  return binomial_coefficient_factor(x, n, k);
}
