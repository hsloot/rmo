#ifndef MO_MATH_MISC_HPP
#define MO_MATH_MISC_HPP

#include <Rcpp.h>

namespace mo {
namespace math {

template<typename T>
inline T min(const T& a, const T& b) {
  if (a == R_PosInf && b == R_PosInf) {
    return R_PosInf;
  } else if (a == R_PosInf) {
    return b;
  } else if (b == R_PosInf) {
    return a;
  }
  return (a < b ? a : b);
}

} // math
} // mo

#endif // MO_MATH_MISC_HPP
