#pragma once

#include <type_traits>

#include "rmolib/bit/internal/make_bitmask.hpp"
#include "rmolib/math/next_integral_value.hpp"

namespace rmolib {

namespace bit {

namespace internal {

template <typename _UnsignedType>
constexpr _UnsignedType bit_fill_ones(const _UnsignedType position_begin,
                                      const _UnsignedType position_end) {
    if (position_begin == position_end)
        return 0;
    else
        return internal::make_bitmask(position_begin) +
               bit_fill_ones(math::next_integral_value(position_begin),
                             position_end);
}

template <typename _UnsignedType>
constexpr _UnsignedType bit_fill_zeros(
    [[maybe_unused]] const _UnsignedType position_begin,
    [[maybe_unused]] const _UnsignedType position_end) {
    return 0;
}

}  // namespace internal

//! returns `_UnsignedType` whose bits between `position_begin` and
//! `position_end-1` are set to 1 if `value==true` or 0 if `value==false`
template <typename _UnsignedType>
constexpr _UnsignedType bit_fill(const _UnsignedType position_begin,
                                 const _UnsignedType position_end,
                                 const bool value = true) {
    static_assert(std::is_unsigned_v<_UnsignedType>,
                  "bit_fill<>: _UnsignedType not unsigned");

    if (value)
        return internal::bit_fill_ones(position_begin, position_end);
    else
        return internal::bit_fill_zeros(position_begin, position_end);
    ;
}

}  // namespace bit

}  // namespace rmolib
