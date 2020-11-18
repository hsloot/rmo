#pragma once

#include <type_traits>

namespace rmolib {

namespace random {

namespace internal {

// type_trait for identifying possible alternative implementations of
// a *_mo_distribution<>::param_type
template <typename _T, class = void>
struct is_mo_param_type : public std::false_type {};

template <typename _T>
struct is_mo_param_type<
    _T, typename std::enable_if<decltype(
            std::declval<_T&>().intensities(),
            std::true_type())::value&& decltype(std::declval<_T&>().dim(),
                                                std::true_type())::value>::type>
    : public std::true_type {};

template <typename _T>
constexpr bool is_mo_param_type_v = is_mo_param_type<_T>::value;

}  // namespace internal

}  // namespace random

}  // namespace rmolib
