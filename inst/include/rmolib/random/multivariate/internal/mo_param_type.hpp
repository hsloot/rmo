#pragma once

#include <type_traits>

namespace rmolib {

namespace random {

namespace internal {

template <typename _T, class = void>
struct __is_mo_param_type : public std::false_type {};

template <typename _T>
struct __is_mo_param_type<
    _T,
    std::enable_if_t<
        decltype(std::declval<_T>().dim(), std::true_type())::value&& decltype(
            std::declval<_T>().intensities(), std::true_type())::value>>
    : public std::true_type {};

}  // namespace internal

template <typename _T>
struct is_mo_param_type
    : public internal::__is_mo_param_type<std::remove_cv_t<_T>> {};

template <typename _T>
constexpr bool is_mo_param_type_v = is_mo_param_type<_T>::value;

}  // namespace random

}  // namespace rmolib
