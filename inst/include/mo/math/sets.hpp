#ifndef MO_MATH_SETS_HPP
#define MO_MATH_SETS_HPP

#include <cstddef>
#include <type_traits>

namespace mo {
namespace math {

template<typename T>
inline bool is_within(const T i, const T j)  {
  static_assert(std::is_unsigned<T>::value, "type not unsigned"); // # nocov
  return ((j+1) >> i) % 2 == 1;
}

} // math
} // mo

#endif // MO_MATH_SETS_HPP
