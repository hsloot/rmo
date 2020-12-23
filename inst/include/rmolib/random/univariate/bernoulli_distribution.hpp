#pragma once

#include <stdexcept>
#include <type_traits>

#include "rmolib/type_traits/is_safe_numeric_cast.hpp"

namespace rmolib {

namespace random {

namespace internal {

template <typename _T, class = void>
struct __is_bernoulli_param_type : public std::false_type {};

template <typename _T>
struct __is_bernoulli_param_type<
    _T,
    std::enable_if_t<decltype(std::declval<_T>().p(), std::true_type())::value>>
    : public std::true_type {};

}  // namespace internal

template <typename _T>
struct is_bernoulli_param_type
    : public internal::__is_bernoulli_param_type<std::remove_cv_t<_T>> {};

//! true, if _T can be used to construct bernoulli_distribution<>::param_type
template <typename _T>
constexpr bool is_bernoulli_param_type_v = is_bernoulli_param_type<_T>::value;

template <typename _IntType, typename _WeightType,
          typename _UnitUniformRealDistribution>
class bernoulli_distribution {
 public:
  using result_type = _IntType;

  class param_type {
   public:
    using distribution_type = bernoulli_distribution;

    param_type() = default;

    explicit param_type(const _WeightType p) : p_{p} { __validate_input(p_); }

    // Used for construction from a different specialization
    template <typename _BernoulliParamType,
              std::enable_if_t<
                  !std::is_convertible_v<_BernoulliParamType, param_type> &&
                      is_bernoulli_param_type_v<_BernoulliParamType>,
                  int> = 0>
    explicit param_type(_BernoulliParamType&& parm) : param_type{parm.p()} {}

    // compiler generated ctor and assignment op is sufficient

    auto p() const { return p_; }

    friend class bernoulli_distribution;

    friend bool operator==(const param_type& lhs, const param_type& rhs) {
      return lhs.p_ == rhs.p_;
    }

    friend bool operator!=(const param_type& lhs, const param_type& rhs) {
      return !(lhs == rhs);
    }

   private:
    _WeightType p_{0.5};

    void __validate_input(const _WeightType p) const {
      if (!(p_ >= _WeightType{0} && p_ <= _WeightType{1}))
        throw std::domain_error("p must be in [0, 1]");
    }

    static_assert(
        std::is_integral_v<_IntType>,
        "Class template rmolib::random::bernoulli_distribution<> must be "
        "parametrized with integral type");
  };

  bernoulli_distribution() { __init_unit_uniform_real_distribution(); }

  explicit bernoulli_distribution(const _WeightType p) : parm_{p} {
    __init_unit_uniform_real_distribution();
  }

  explicit bernoulli_distribution(const param_type& parm) : parm_{parm} {
    __init_unit_uniform_real_distribution();
  }

  // Used for construction from a different specialization
  template <
      typename _BernoulliParamType,
      std::enable_if_t<
          !std::is_convertible_v<_BernoulliParamType, bernoulli_distribution> &&
              !std::is_convertible_v<_BernoulliParamType, param_type> &&
              is_bernoulli_param_type_v<_BernoulliParamType>,
          int> = 0>
  explicit bernoulli_distribution(_BernoulliParamType&& parm)
      : parm_{std::forward<_BernoulliParamType>(parm)} {
    __init_unit_uniform_real_distribution();
  }

  // compiler generated ctor and assignment op is sufficient

  void reset() {}

  auto min() const { return _WeightType{0}; }
  auto max() const { return _WeightType{1}; }

  auto p() const { return parm_.p(); }

  param_type param() const { return parm_; }
  void param(const param_type& parm) { parm_ = parm; }

  template <typename _Engine>
  result_type operator()(_Engine&& engine) {
    return (*this)(std::forward<_Engine>(engine), parm_);
  }

  template <typename _Engine>
  result_type operator()(_Engine&& engine, param_type parm) {
    if (0 == parm.p_)
      return 0;
    else if (1 == parm.p_)
      return 1;

    const auto u = unit_uniform_real_dist_(std::forward<_Engine>(engine));
    if (u < parm.p_)
      return 0;
    else
      return 1;
  }

  friend bool operator==(const bernoulli_distribution& lhs,
                         const bernoulli_distribution& rhs) {
    return lhs.parm_ == rhs.parm_;
  }

  friend bool operator!=(const bernoulli_distribution& lhs,
                         const bernoulli_distribution& rhs) {
    return !(lhs == rhs);
  }

 private:
  param_type parm_{};
  _UnitUniformRealDistribution unit_uniform_real_dist_{};

  void __init_unit_uniform_real_distribution() {
    if constexpr (std::is_constructible_v<_UnitUniformRealDistribution,
                                          const _WeightType,
                                          const _WeightType>) {
      // TODO:
      // static_assert(is_distribution_type<_UnitUniformRealDistribution>)
      using unit_param_type = typename _UnitUniformRealDistribution::param_type;
      unit_uniform_real_dist_.param(unit_param_type{0., 1.});
    }
  }

  static_assert(
      type_traits::is_safe_numeric_cast_v<
          _WeightType, typename _UnitUniformRealDistribution::result_type>,
      "Class template rmolib::random::bernoulli_distribution<> must be "
      "parametrized with unit_uniform_real_distribution-type with matching "
      "result_type and _WeightType");
};

/*
  // TODO: implement

  template <class _CharType, class _Traits, typename _IntType, typename
  _WeightType, typename _UnitUniformRealDistribution>
  std::basic_ostream<_CharType, _Traits>&
  operator<<(std::basic_ostream<_CharType, _Traits>& os,
            bernoulli_distribution<_IntType, _WeightType,
  _UnitUniformRealDistribution>& dist);

  template <class _CharType, class _Traits, typename _IntType, typename
  _WeightType, typename _UnitUniformRealDistribution>
  std::basic_istream<_CharType, _Traits>&
  operator>>(std::basic_istream<_CharType, _Traits>& is,
             bernoulli_distribution<_IntType, _WeightType,
  _UnitUniformRealDistribution>& dist);
*/

}  // namespace random

}  // namespace rmolib
