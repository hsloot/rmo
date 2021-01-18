#include <Rcpp.h>
#include <rmolib/math/binomial_coefficient.hpp>

// [[Rcpp::export]]
double multiply_binomial_coefficient(const double x, const std::size_t n,
                                     const std::size_t k) {
  return rmolib::math::multiply_binomial_coefficient(x, n, k);
}

// [[Rcpp::export]]
double divide_binomial_coefficient(const double x, const std::size_t n,
                                   const std::size_t k) {
  return rmolib::math::multiply_binomial_coefficient(x, n, k,
                                                     std::divides<double>{});
}
