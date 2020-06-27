#ifndef MO_MATH_SETS_HPP
#define MO_MATH_SETS_HPP

#include <cstddef>

namespace mo {
namespace math {

inline bool is_within(const std::size_t i, const std::size_t j)  {
  return ((j+1) >> i) % 2 == 1;
}

} // math
} // mo

#endif // MO_MATH_SETS_HPP
