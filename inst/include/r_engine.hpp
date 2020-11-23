#pragma once

#include <cmath>

#include <Rcpp.h>
#include <rmolib/random/univariate/exponential_distribution.hpp>
#include <rmolib/random/univariate/uniform_int_distribution.hpp>
#include <rmolib/random/univariate/uniform_real_distribution.hpp>

struct r_engine {};

// ----------------------------------------
// implementation details below
// ----------------------------------------

namespace rmolib {
namespace random {

template <>
template <>
inline double uniform_real_distribution<double>::unit_uniform_real_distribution(
    r_engine& engine) {
  return ::unif_rand();
}

template <>
template <>
inline double exponential_distribution<double>::unit_exponential_distribution(
    r_engine& engine) {
  return ::exp_rand();
}

template <>
template <>
inline int uniform_int_distribution<int>::unit_uniform_int_distribution(
    r_engine& engine, const int n) {
#if defined(R_VERSION) && R_VERSION >= R_Version(3, 4, 0)
  return ::R_unif_index(static_cast<double>(n));
#else
  /*
    Sample cannot be reimplemented fully backwards compatible because
    of logic changes in between R 3.3 and R 3.4. However, as long
    as the sample population and the number of samples are smaller
    than `INT_MAX`, this emulation should yield the same results.
   */
  return std::floor(static_cast<double>(n * ::unif_rand()));
#endif
}

}  // namespace random
}  // namespace rmolib
