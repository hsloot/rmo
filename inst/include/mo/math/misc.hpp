#ifndef MO_MATH_MISC_HPP
#define MO_MATH_MISC_HPP

#include <stdexcept>
#include <limits>

namespace mo {
namespace math {

inline double min(const double a, const double b) {
  if (
      std::numeric_limits<double>::infinity() == a &&
      std::numeric_limits<double>::infinity() == b) {
    return std::numeric_limits<double>::infinity(); // # nocov
  } else if (std::numeric_limits<double>::infinity() == a) {
    return b;
  } else if (std::numeric_limits<double>::infinity() == b) {
    return a;
  }
  return (a < b ? a : b);
}

template<typename T>
inline T binomial_coefficient(const T n, const T k) {
  if (k > n) std::range_error("k > n"); // # nocov
  if (0 == k) return 1;
  if (k > (n/2)) return binomial_coefficient(n, n-k);

  return n * binomial_coefficient(n-1, k-1) / k;
}

} // math
} // mo

#endif // MO_MATH_MISC_HPP
