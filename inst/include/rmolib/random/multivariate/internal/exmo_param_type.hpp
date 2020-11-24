#pragma once

#include <type_traits>

namespace rmolib {

namespace random {

namespace internal {

// type_trait for identifying possible alternative implementations of
// a *_exmo_distribution<>::param_type
template <typename _T, class = void>
struct is_exmo_param_type : public std::false_type {};

template <typename _T>
struct is_exmo_param_type<
    _T,
    std::enable_if_t<
        decltype(std::declval<_T&>().dim(), std::true_type())::value&& decltype(
            std::declval<_T&>().ex_intensities(), std::true_type())::value>>
    : public std::true_type {};

template <typename _T>
constexpr bool is_exmo_param_type_v = is_exmo_param_type<_T>::value;

}  // namespace internal

}  // namespace random

}  // namespace rmolib
