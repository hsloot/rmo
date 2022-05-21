#include <algorithm>
#include <cstddef>
#include <utility>

#include <Rcpp.h>

#include "rmolib/math/binomial_coefficient.hpp"

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector multiply_binomial_coefficient(const NumericVector& x,
                                            const std::size_t n,
                                            const std::size_t k) {
    auto out = clone(x);
    std::transform(out.cbegin(), out.cend(), out.begin(), [n, k](const auto x) {
        return rmolib::math::multiply_binomial_coefficient(x, n, k);
    });
    return out;
}

// [[Rcpp::export]]
NumericVector divide_binomial_coefficient(const NumericVector& x,
                                          const std::size_t n,
                                          const std::size_t k) {
    auto out = clone(x);
    std::transform(out.cbegin(), out.cend(), out.begin(), [n, k](const auto x) {
        return rmolib::math::multiply_binomial_coefficient(
            x, n, k, std::divides<double>{});
    });
    return out;
}
