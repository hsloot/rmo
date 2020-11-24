#pragma once

#include <limits>
#include <type_traits>

namespace rmolib {

namespace type_traits {

template <typename _IntTypeBase, typename _IntTypDeriv, class = void>
struct is_integral_base_of : public std::false_type {};

template <typename _IntTypeBase, typename _IntTypeDeriv>
struct is_integral_base_of<
    _IntTypeBase, _IntTypeDeriv,
    std::enable_if_t<std::is_integral_v<_IntTypeBase> &&
                     std::is_integral_v<_IntTypeDeriv> &&
                     (std::numeric_limits<_IntTypeDeriv>::max() >=
                      std::numeric_limits<_IntTypeBase>::max()) &&
                     (std::is_unsigned_v<_IntTypeBase> ||
                      std::is_signed_v<_IntTypeDeriv>)>>
    : public std::true_type {};

template <typename _IntTypeBase, typename _IntTypeDeriv>
constexpr bool is_integral_base_of_v = is_integral_base_of<_IntTypeBase, _IntTypeDeriv>::value;


}  // namespace type_traits

}  // namespace rmolib
