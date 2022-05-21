#pragma once

#include <type_traits>

#include "rmolib/bit/bit_test.hpp"

namespace rmolib {

namespace random {

namespace internal {

//! true, if element i is in set j (<=> if i'th bit in j is set)
template <typename _UnsignedType>
inline bool is_within(const _UnsignedType i, const _UnsignedType j) {
    static_assert(std::is_unsigned_v<_UnsignedType>,
                  "is_within<>: _UnsignedType not unsigned");
    return bit::bit_test(j, i);
}

}  // namespace internal

}  // namespace random

}  // namespace rmolib
