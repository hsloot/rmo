#pragma once

#include <cmath>
#include <limits>
#include <stdexcept>
#include <type_traits>

namespace rmolib {

namespace random {

// type_trait for identifying possible alternative implementations of
// a uniform_real_distribution<>::param_type
template <typename _T, class = void>
struct is_uniform_real_param_type : public std::false_type {};

template <typename _T>
struct is_uniform_real_param_type<
    _T, typename std::enable_if<decltype(
            std::declval<_T&>().lower(),
            std::true_type())::value&& decltype(std::declval<_T&>().upper(),
                                                std::true_type())::value>::type>
    : public std::true_type {};

template <typename _T>
constexpr bool is_uniform_real_param_type_v =
    is_uniform_real_param_type<_T>::value;

template <typename _RealType>
class uniform_real_distribution {
 public:
  using result_type = _RealType;

  class param_type {
   public:
    using distribution_type = uniform_real_distribution;

    param_type() = default;

    explicit param_type(const _RealType lower, const _RealType upper)
        : lower_{lower}, length_{upper - lower} {
      __validate_input(lower_, lower_ + length_);
    }

    // Used for construction from a different specialization
    template <typename _UniformParamType,
              typename std::enable_if<
                  !std::is_convertible_v<_UniformParamType, param_type> &&
                      is_uniform_real_param_type_v<_UniformParamType>,
                  int>::type = 0>
    explicit param_type(_UniformParamType&& param)
        : param_type{param.lower(), param.upper()} {}

    // compiler generated ctor and assignment op is sufficient

    auto lower() const { return lower_; }
    auto upper() const { return lower_ + length_; }

    friend class uniform_real_distribution;

    friend bool operator==(const param_type& lhs, const param_type& rhs) {
      return lhs.lower_ == rhs.lower_ && lhs.length_ == rhs.length_;
    }

    friend bool operator!=(const param_type& lhs, const param_type& rhs) {
      return !(lhs == rhs);
    }

   private:
    _RealType lower_{0.};
    _RealType length_{1.};

    void __validate_input(const _RealType lower,
                          const _RealType upper) const {
      auto is_finite = [](const auto x) {
        return std::abs(x) < std::numeric_limits<_RealType>::infinity();
      };

      if (!(is_finite(lower) && is_finite(upper)))
        throw std::domain_error("lower and upper must be in (-infty, infty)");

      if (upper <= lower)
        throw std::domain_error("upper - lower must be positive");
    }

    static_assert(
        std::is_floating_point<_RealType>::value &&
            std::numeric_limits<_RealType>::is_iec559,
        "Class template rmolib::random::uniform_real_distribution<> must be "
        "parametrized with IEEE 759 conformant floating-point number");
  };

  uniform_real_distribution() = default;

  explicit uniform_real_distribution(const _RealType lower,
                                     const _RealType upper)
      : param_{lower, upper} {}
  uniform_real_distribution(const param_type& param) : param_{param} {}

  // compiler generated ctor and assignment op is sufficient

  void reset() {}

  auto min() const { return lower(); }
  auto max() const { return upper(); }

  auto lower() const { return param_.lower(); }
  auto upper() const { return param_.upper(); }

  param_type param() const { return param_; }
  void param(const param_type& param) { param_ = param; }

  template <typename _EngineType>
  result_type operator()(_EngineType& engine) {
    return (*this)(engine, param_);
  }

  template <typename _EngineType>
  result_type operator()(_EngineType& engine, const param_type& param) {
      return param.lower_ +
             param.length_ * unit_uniform_real_distribution(engine);
  }

  friend bool operator==(const uniform_real_distribution& lhs,
                         const uniform_real_distribution& rhs) {
    return lhs.param_ == rhs.param_;
  }

  friend bool operator!=(const uniform_real_distribution& lhs,
                         const uniform_real_distribution& rhs) {
    return lhs.param_ != rhs.param_;
  }

 private:
  param_type param_{};

  template <typename _EngineType>
  result_type unit_uniform_real_distribution(_EngineType& engine);
};

/*
  // TODO: implement

  template <class _CharType, class _Traits, class _RealType>
  std::basic_ostream<_CharType, _Traits>&
  operator<<(std::basic_ostream<_CharType, _Traits>& os,
            uniform_real_distribution<_RealType>& dist);

  template <class _CharType, class _Traits, class _RealType>
  std::basic_istream<_CharType, _Traits>&
  operator>>(std::basic_istream<_CharType, _Traits>& is,
             uniform_real_distribution<_RealType>& dist);
*/

}  // namespace random

}  // namespace rmolib
