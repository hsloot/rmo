#pragma once

#include <type_traits>

namespace rmolib {

namespace random {

namespace internal {

template <typename _UnsignedType>
inline bool is_within(const _UnsignedType i, const _UnsignedType j) {
  static_assert(std::is_unsigned<_UnsignedType>::value,
                "_UnsignedType not unsigned");
  return ((j + 1) >> i) % 2 == 1;
}

}  // namespace internal

}  // namespace random

}  // namespace rmolib
