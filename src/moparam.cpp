#include <algorithm>
#include <cstddef>
#include <iterator>
#include <numeric>

#include <Rcpp.h>

#include "rmolib/random/multivariate/internal/is_within.hpp"

using namespace Rcpp;

//! unscaled `uex_intensities` to `intensities`
// [[Rcpp::export(rng=false)]]
NumericVector uexi2i(const NumericVector& uexi) {
  const auto d = static_cast<std::size_t>(uexi.size());
  auto out = NumericVector(no_init((1 << d) - 1));
  for (auto j = std::size_t{0}; j < out.size(); ++j) {
    auto cnt = std::size_t{0};
    for (auto i = std::size_t{0}; i < d; ++i) {
      if (rmolib::random::internal::is_within(i, j + 1)) ++cnt;
    }
    out[j] = uexi[cnt - 1];
  }

  return out;
}

//! scaled `ex_intensities` to `ex_qmatrix`
// [[Rcpp::export(rng=false)]]
NumericMatrix exi2exqm(const NumericVector& exi) {
  const auto d = static_cast<std::size_t>(exi.size());
  auto out = NumericMatrix(no_init(d + 1, d + 1));
  for (auto i = std::size_t{0}; i <= d; ++i) {
    auto values = out(i, _);
    std::fill(values.begin(), std::next(values.begin(), i + 1), 0.);
    if (0 < i) {
      for (auto j = i + 1; j <= d; ++j) {
        values[j] = static_cast<double>(d - j + 1) /
                        static_cast<double>(d - i + 1) * out(i - 1, j - 1) +
                    static_cast<double>(j + 1 - i) /
                        static_cast<double>(d - i + 1) * out(i - 1, j);
      }
    } else if (0 == i) {
      std::copy(exi.cbegin(), exi.cend(), ++values.begin());
    }
    values[i] =
        -std::accumulate(std::next(values.cbegin(), i + 1), values.cend(), 0.);
  }

  return out;
}
