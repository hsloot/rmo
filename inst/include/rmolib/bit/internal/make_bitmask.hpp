#pragma once

#include <type_traits>

namespace rmolib {

namespace bit {

namespace internal {

template <typename _UnsignedType>
constexpr _UnsignedType make_bitmask(const _UnsignedType pos) {
    static_assert(std::is_unsigned_v<_UnsignedType>,
                  "make_bitmask<>: _UnsignedType not unsigned");

    return _UnsignedType{1} << pos;
}

}  // namespace internal

}  // namespace bit

}  // namespace rmolib
