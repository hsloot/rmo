#ifndef MO_MATH_SETS_HPP
#define MO_MATH_SETS_HPP

namespace mo {
namespace math {

template<typename T>
inline bool is_within(const T &i, const T &j)  {
  return ((j+1) >> i) % 2 == 1;
} 

} // math
} // mo

#endif // MO_MATH_SETS_HPP
