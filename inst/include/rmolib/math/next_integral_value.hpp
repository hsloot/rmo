#pragma once

#include<type_traits>

namespace rmolib {

namespace math {

template <typename _IntVal>
constexpr _IntVal next_integral_value(_IntVal i) {
  return ++i;
}

}  // namespace math

}  // namespace rmolib
