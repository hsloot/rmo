#pragma once

#include <cmath>

#include <R_ext/Random.h>
#include <Rmath.h>
#include <Rversion.h>

struct r_engine {};

// ----------------------------------------
// implementation details below
// ----------------------------------------

namespace rmolib {

namespace random {

namespace internal {

template <typename _RealType, typename _Engine>
inline _RealType unit_uniform_real_distribution(
    [[maybe_unused]] _Engine&& engine, r_engine) {
  return ::unif_rand();
}

template <typename _RealType, typename _Engine>
inline _RealType unit_exponential_distribution(
    [[maybe_unused]] _Engine&& engine, r_engine) {
  return ::exp_rand();
}

template <typename _IntType, typename _Engine>
inline _IntType unit_uniform_int_distribution([[maybe_unused]] _Engine&& engine,
                                              const _IntType n, r_engine) {
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

}  // namespace internal

}  // namespace random

}  // namespace rmolib
