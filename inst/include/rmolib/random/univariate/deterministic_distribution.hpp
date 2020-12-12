#pragma once

#include <limits>
#include <type_traits>

namespace rmolib {

namespace random {

namespace internal {

template <typename _T, class = void>
struct __is_deterministic_param_type : public std::false_type {};

template <typename _T>
struct __is_deterministic_param_type<
    _T, std::enable_if_t<decltype(std::declval<_T>().value(),
                                  std::true_type())::value>>
    : public std::true_type {};

}  // namespace internal

template <typename _T>
struct is_deterministic_param_type
    : public internal::__is_deterministic_param_type<std::remove_cv_t<_T>> {};

//! true, if _T can be used to construct
//! deterministic_distribution<>::param_type
template <typename _T>
constexpr bool is_deterministic_param_type_v =
    is_deterministic_param_type<_T>::value;

template <typename _RealType>
class deterministic_distribution {
 public:
  using result_type = _RealType;

  class param_type {
   public:
    using distribution_type = deterministic_distribution;

    param_type() = default;

    explicit param_type(const _RealType value) : value_{value} {
      __validate_input(value_);
    }

    // Used for construction from a different specialization
    template <typename _DeterministicParamType,
              std::enable_if_t<
                  !std::is_convertible_v<_DeterministicParamType, param_type> &&
                      is_deterministic_param_type_v<_DeterministicParamType>,
                  int> = 0>
    explicit param_type(_DeterministicParamType&& parm)
        : param_type{parm.value()} {}

    // compiler generated ctor and assignment op is sufficient

    auto value() const { return value_; }

    friend class deterministic_distribution;

    friend bool operator==(const param_type& lhs, const param_type& rhs) {
      return lhs.value_ == rhs.value_;
    }

    friend bool operator!=(const param_type& lhs, const param_type& rhs) {
      return !(lhs == rhs);
    }

   private:
    _RealType value_{1.};

    void __validate_input(const _RealType value) const {}

    static_assert(
        std::is_floating_point_v<_RealType> &&
            std::numeric_limits<_RealType>::is_iec559,
        "Class template rmolib::random::deterministic_distribution<> must be "
        "parametrized with IEEE 759 conformant floating-point number");
  };

  deterministic_distribution() = default;

  explicit deterministic_distribution(const _RealType value) : parm_{value} {}

  explicit deterministic_distribution(const param_type& parm) : parm_{parm} {}

  // Used for construction from a different specialization
  template <
      typename _DeterministicParamType,
      std::enable_if_t<
          !std::is_convertible_v<_DeterministicParamType,
                                 deterministic_distribution> &&
              !std::is_convertible_v<_DeterministicParamType, param_type> &&
              is_deterministic_param_type_v<_DeterministicParamType>,
          int> = 0>
  explicit deterministic_distribution(_DeterministicParamType&& parm)
      : parm_{std::forward<_DeterministicParamType>(parm)} {}

  // compiler generated ctor and assignment op is sufficient

  void reset() {}

  auto min() const { return value(); }
  auto max() const { return value(); }

  auto value() const { return parm_.value(); }

  param_type param() const { return parm_; }
  void param(const param_type& parm) { parm_ = parm; }

  template <typename _Engine>
  result_type operator()(_Engine&& engine) {
    return (*this)(std::forward<_Engine>(engine), parm_);
  }

  template <typename _Engine>
  result_type operator()(_Engine&& engine, const param_type& parm) {
    return parm.value_;
  }

  friend bool operator==(const deterministic_distribution& lhs,
                         const deterministic_distribution& rhs) {
    return lhs.parm_ == rhs.parm_;
  }

  friend bool operator!=(const deterministic_distribution& lhs,
                         const deterministic_distribution& rhs) {
    return !(lhs == rhs);
  }

 private:
  param_type parm_{};
};

/*
  // TODO: implement

  template <class _CharType, class _Traits, class _RealType>
  std::basic_ostream<_CharType, _Traits>&
  operator<<(std::basic_ostream<_CharType, _Traits>& os,
            deterministic_distribution<_RealType>& dist);

  template <class _CharType, class _Traits, class _RealType>
  std::basic_istream<_CharType, _Traits>&
  operator>>(std::basic_istream<_CharType, _Traits>& is,
             deterministic_distribution<_RealType>& dist);
*/

}  // namespace random

}  // namespace rmolib
