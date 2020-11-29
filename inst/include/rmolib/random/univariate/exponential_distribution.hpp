#pragma once

#include <limits>
#include <stdexcept>
#include <type_traits>

namespace rmolib {

namespace random {

namespace internal {

template <typename _T, class = void>
struct __is_exponential_param_type : public std::false_type {};

template <typename _T>
struct __is_exponential_param_type<
    _T, std::enable_if_t<decltype(std::declval<_T>().lambda(),
                                  std::true_type())::value>>
    : public std::true_type {};

}  // namespace internal

template <typename _T>
struct is_exponential_param_type
    : public internal::__is_exponential_param_type<std::remove_cv_t<_T>> {};

template <typename _T>
constexpr bool is_exponential_param_type_v =
    is_exponential_param_type<_T>::value;

template <typename _RealType>
class exponential_distribution {
 public:
  using result_type = _RealType;

  class param_type {
   public:
    using distribution_type = exponential_distribution;

    param_type() = default;
    explicit param_type(const _RealType lambda) : lambda_{lambda} {
      __validate_input(lambda_);
    }

    // Used for construction from a different specialization
    template <typename _ExponentialParamType,
              std::enable_if_t<
                  !std::is_convertible_v<_ExponentialParamType, param_type> &&
                      is_exponential_param_type_v<_ExponentialParamType>,
                  int> = 0>
    explicit param_type(_ExponentialParamType&& parm)
        : param_type{parm.lambda()} {}

    // compiler generated ctor and assignment op is sufficient

    auto lambda() const { return lambda_; }

    friend class exponential_distribution;

    friend bool operator==(const param_type& lhs, const param_type& rhs) {
      return lhs.lambda_ == rhs.lambda_;
    }

    friend bool operator!=(const param_type& lhs, const param_type& rhs) {
      return !(lhs == rhs);
    }

   private:
    _RealType lambda_{1.};

    void __validate_input(const double lambda) const {
      if (lambda < 0) throw std::domain_error("lambda_ must be positive");
    }

    static_assert(
        std::is_floating_point_v<_RealType> &&
            std::numeric_limits<_RealType>::is_iec559,
        "Class template rmolib::random::exponential_distribution<> must be "
        "parametrized with IEEE 759 conformant floating-point number");
  };

  exponential_distribution() = default;

  explicit exponential_distribution(const _RealType lambda) : parm_{lambda} {}

  explicit exponential_distribution(const param_type& parm) : parm_{parm} {}

  // Used for construction from a different specialization
  template <typename _ExponentialParamType,
            std::enable_if_t<
                !std::is_convertible_v<_ExponentialParamType,
                                       exponential_distribution> &&
                    !std::is_convertible_v<_ExponentialParamType, param_type> &&
                    is_exponential_param_type_v<_ExponentialParamType>,
                int> = 0>
  explicit exponential_distribution(_ExponentialParamType&& parm)
      : parm_{std::forward<_ExponentialParamType>(parm)} {}

  // compiler generated ctor and assignment op is sufficient

  void reset() {}

  auto min() const { return result_type{0}; }
  auto max() const { return std::numeric_limits<result_type>::infinity(); }

  auto lambda() const { return parm_.lambda(); }

  param_type param() const { return parm_; }
  void param(const param_type& parm) { parm_ = parm; }

  template <typename _Engine>
  result_type operator()(_Engine& engine) {
    return (*this)(engine, parm_);
  }

  template <typename _Engine>
  result_type operator()(_Engine& engine, const param_type& parm) {
    if (_RealType{0} == parm.lambda_)
      return std::numeric_limits<_RealType>::infinity();
    else if (std::numeric_limits<_RealType>::infinity() == parm.lambda_)
      return _RealType{0};
    else
      return unit_exponential_distribution(engine) / parm.lambda_;
  }

  friend bool operator==(const exponential_distribution& lhs,
                         const exponential_distribution& rhs) {
    return lhs.parm_ == rhs.parm_;
  }

  friend bool operator!=(const exponential_distribution& lhs,
                         const exponential_distribution& rhs) {
    return !(lhs == rhs);
  }

 private:
  param_type parm_{};

  template <typename _Engine>
  result_type unit_exponential_distribution(_Engine& engine);
};

/*
  // TODO: implement

  template <class _CharType, class _Traits, class _RealType>
  std::basic_ostream<_CharType, _Traits>&
  operator<<(std::basic_ostream<_CharType, _Traits>& os,
            exponential_distribution<_RealType>& dist);

  template <class _CharType, class _Traits, class _RealType>
  std::basic_istream<_CharType, _Traits>&
  operator>>(std::basic_istream<_CharType, _Traits>& is,
             exponential_distribution<_RealType>& dist);
*/

}  // namespace random

}  // namespace rmolib
