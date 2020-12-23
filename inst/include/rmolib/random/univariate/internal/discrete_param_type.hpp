#pragma once

#include <type_traits>

namespace rmolib {

namespace random {

namespace internal {

template <typename _T, class = void>
struct __is_discrete_param_type : public std::false_type {};

template <typename _T>
struct __is_discrete_param_type<
    _T, std::enable_if_t<decltype(std::declval<_T>().probabilities(),
                                  std::true_type())::value>>
    : public std::true_type {};

}  // namespace internal

template <typename _T>
struct is_discrete_param_type
    : public internal::__is_discrete_param_type<std::remove_cv_t<_T>> {};

//! true, if _T can be used to construct r_discrete_distribution<>::param_type
template <typename _T>
constexpr bool is_discrete_param_type_v = is_discrete_param_type<_T>::value;

}  // namespace random

}  // namespace rmolib
