#pragma once

#include <functional>
#include <stdexcept>
#include <type_traits>
#include <utility>

namespace rmolib {

namespace math {

template <typename _RealType, typename _IntType, typename _BinaryOperation>
inline _RealType multiply_binomial_coefficient(const _RealType x,
                                               const _IntType n,
                                               const _IntType k,
                                               _BinaryOperation binary_op) {
  static_assert(
      decltype(binary_op(std::declval<const _RealType>(),
                         std::declval<const _RealType>()),
               std::true_type())::value&&
          std::is_same_v<_RealType,
                         decltype(binary_op(std::declval<const _RealType>(),
                                            std::declval<const _RealType>()))>,
      "multiply_binomial_coefficient<>: _BinaryOperation has wrong signature");
  static_assert(std::is_integral_v<_IntType>,
                "multiply_binomial_coefficient<>: _IntType not intgral value");
  static_assert(std::is_floating_point_v<_RealType>,
                "multiply_binomial_coefficient<>: _RealType not intgral value");
  if (n < 0)
    std::domain_error(
        "multiply_binomial_coefficient<> cannot be called with negative n");

  if (k > n || k < 0)
    return _RealType{0};
  else if (0 == k)
    return x;
  else if (k > (n / 2))
    return multiply_binomial_coefficient(x, n, n - k, binary_op);
  else
    return binary_op(multiply_binomial_coefficient(x, n - 1, k - 1, binary_op),
                     static_cast<_RealType>(n) / static_cast<_RealType>(k));
}

template <typename _RealType, typename _IntType>
inline _RealType multiply_binomial_coefficient(const _RealType x,
                                               const _IntType n,
                                               const _IntType k) {
  return multiply_binomial_coefficient(x, n, k, std::multiplies<_RealType>{});
}

template <typename _IntType>
inline _IntType binomial_coefficient(const _IntType n, const _IntType k) {
  return static_cast<_IntType>(multiply_binomial_coefficient(1., n, k));
}

}  // namespace math

}  // namespace rmolib
