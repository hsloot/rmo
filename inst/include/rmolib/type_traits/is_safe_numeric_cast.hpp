#pragma once

#include <limits>
#include <type_traits>

namespace rmolib {

namespace type_traits {

namespace internal {

template <typename... Conds> // cSpell:ignore Conds
struct and_ : std::true_type {};

template <typename Cond, typename... Conds>
struct and_<Cond, Conds...>
    : std::conditional_t<Cond::value, and_<Conds...>, std::false_type> {};

template <typename _T, typename _S>
struct is_same_decayed
    : std::integral_constant<
          bool, std::is_same_v<std::decay_t<_T>, std::decay_t<_S>>> {};
template <class _T, class _S>
constexpr bool is_same_decayed_v = is_same_decayed<_T, _S>::value;

template <typename _T>
struct is_numeric
    : public std::integral_constant<bool, std::is_arithmetic_v<_T> &&
                                              !is_same_decayed_v<bool, _T>> {};
template <typename _T>
constexpr bool is_numeric_v = is_numeric<_T>::value;

template <typename... _Ts>
using are_all_numeric = and_<is_numeric<_Ts>...>;
template <typename... _Ts>
constexpr bool are_all_numeric_v = are_all_numeric<_Ts...>::value;

template <typename... _Ts>
using are_all_floating_point = and_<std::is_floating_point<_Ts>...>;
template <typename... _Ts>
constexpr bool are_all_floating_point_v = are_all_floating_point<_Ts...>::value;

template <typename... _Ts>
using are_all_integral = and_<std::is_integral<_Ts>...>;
template <typename... _Ts>
constexpr bool are_all_integral_v = are_all_integral<_Ts...>::value;

template <typename... _Ts>
using are_all_signed = and_<std::is_signed<_Ts>...>;
template <typename... _Ts>
constexpr bool are_all_signed_v = are_all_signed<_Ts...>::value;

template <typename... _Ts>
using are_all_unsigned = and_<std::is_unsigned<_Ts>...>;
template <typename... _Ts>
constexpr bool are_all_unsigned_v = are_all_unsigned<_Ts...>::value;

template <typename... _Ts>
struct have_all_same_signage
    : public std::integral_constant<bool, are_all_signed_v<_Ts...> ||
                                              are_all_unsigned_v<_Ts...>> {};
template <typename... _Ts>
constexpr bool have_all_same_signage_v = have_all_same_signage<_Ts...>::value;

}  // namespace internal

//! true, if cast can be performed without loss of precision; signage might be
//! lost if cast is to unsigned type
template <typename _To, typename _From>
struct is_safe_numeric_cast
    : public std::integral_constant<
          bool, internal::are_all_numeric_v<_To, _From> &&
                    ((std::is_floating_point_v<_To> &&
                      (std::is_integral_v<_From> ||
                       std::numeric_limits<_To>::digits >=
                           std::numeric_limits<_From>::digits)) ||
                     (std::numeric_limits<_To>::digits >
                          std::numeric_limits<_From>::digits ||
                      (internal::have_all_same_signage_v<_To, _From> &&
                       std::numeric_limits<_To>::digits ==
                           std::numeric_limits<_From>::digits)))> {};
template <typename _To, typename _From>
constexpr bool is_safe_numeric_cast_v = is_safe_numeric_cast<_To, _From>::value;

}  // namespace type_traits

}  // namespace rmolib
