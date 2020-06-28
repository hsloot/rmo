#ifndef MO_MATH_MISC_HPP
#define MO_MATH_MISC_HPP

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

} // math
} // mo

#endif // MO_MATH_MISC_HPP
