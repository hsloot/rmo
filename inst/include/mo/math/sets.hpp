#ifndef MO_MATH_SETS_HPP
#define MO_MATH_SETS_HPP

#include <Rinternals.h> // for R_xlen_t

namespace mo {
namespace math {

inline bool is_within(const int& i, const R_xlen_t& j)  {
  return ((j+1) >> i) % 2 == 1;
}

} // math
} // mo

#endif // MO_MATH_SETS_HPP
