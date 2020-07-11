#ifndef MO_MATH_MISC_HPP
#define MO_MATH_MISC_HPP

#include <limits>
#include <stdexcept>

namespace mo {
namespace math {

template <typename T>
inline T min(const T a, const T b) {
  static_assert(std::numeric_limits<T>::is_iec559,  // # nocov
                "type not IEEE 754 conformant");    // # nocov
  if (std::numeric_limits<T>::infinity() == a &&
      std::numeric_limits<T>::infinity() == b) {
    return std::numeric_limits<T>::infinity();  // # nocov
  } else if (std::numeric_limits<T>::infinity() == a) {
    return b;
  } else if (std::numeric_limits<T>::infinity() == b) {
    return a;
  }
  return (a < b ? a : b);
}

template <typename T>
inline T binomial_coefficient(const T n, const T k) {
  if (k > n) throw std::domain_error("k > n");  // # nocov
  if (0 == k) return 1;
  if (k > (n / 2)) return binomial_coefficient(n, n - k);

  return n * binomial_coefficient(n - 1, k - 1) / k;
}

}  // namespace math
}  // namespace mo

#endif  // MO_MATH_MISC_HPP
