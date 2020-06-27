#ifndef MO_MATH_MISC_HPP
#define MO_MATH_MISC_HPP

#include <limits>
#include <Rcpp.h>

namespace mo {
namespace math {

template<typename T>
inline T min(const T a, const T b) {
  if (
      std::numeric_limits<double>::infinity() == a &&
      std::numeric_limits<double>::infinity() == b) {
    return std::numeric_limits<double>::infinity();
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
