#pragma once

#include <type_traits>

#include "rmolib/bit/internal/make_bitmask.hpp"

namespace rmolib {

namespace bit {

template <typename _UnsignedType>
constexpr bool bit_test(const _UnsignedType bitseq,
                        const _UnsignedType position) {
  static_assert(std::is_unsigned_v<_UnsignedType>,
                "has_bit_at_position<>: _UnsignedType not unsigned");

  return bitseq & internal::make_bitmask(position);
}

}  // namespace bit

}  // namespace rmolib
