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
struct is_pareto_param_type : public std::false_type {};

template <typename _T>
struct is_pareto_param_type<
    _T,
    typename std::enable_if<decltype(
        std::declval<_T&>().alpha(),
        std::true_type())::value&& decltype(std::declval<_T&>().lower_bound(),
                                            std::true_type())::value>::type>
    : public std::true_type {};

template <typename _T>
constexpr bool is_pareto_param_type_v = is_pareto_param_type<_T>::value;

template <typename _RealType, typename _UnitUniformRealDistributionType>
class pareto_distribution {
 public:
  using result_type = _RealType;

  class param_type {
   public:
    using distribution_type = pareto_distribution;

    param_type() = default;

    explicit param_type(const _RealType alpha, const _RealType lower_bound)
        : alpha_{alpha}, lower_bound_{lower_bound} {
      __validate_input(alpha_, lower_bound_);
    }

    // Used for construction from a different specialization
    template <typename _ParetoParamType,
              typename std::enable_if<
                  !std::is_convertible_v<_ParetoParamType, param_type> &&
                      is_pareto_param_type_v<_ParetoParamType>,
                  int>::type = 0>
    explicit param_type(_ParetoParamType&& param)
        : param_type{param.alpha(), param.lower_bound()} {}

    // compiler generated ctor and assignment op is sufficient

    auto alpha() const { return alpha_; }
    auto lower_bound() const { return lower_bound_; }

    friend class pareto_distribution;

    friend bool operator==(const param_type& lhs, const param_type& rhs) {
      return lhs.alpha_ == rhs.alpha_ && lhs.lower_bound_ == rhs.lower_bound_;
    }

    friend bool operator!=(const param_type& lhs, const param_type& rhs) {
      return !(lhs == rhs);
    }

   private:
    _RealType alpha_{1.};
    _RealType lower_bound_{1.};

    void __validate_input(const _RealType alpha,
                          const _RealType lower_bound) const {
      auto is_finite = [](auto x) {
        return std::abs(x) < std::numeric_limits<_RealType>::infinity();
      };

      if (!(is_finite(alpha) && (alpha > 0) && is_finite(lower_bound) &&
            (lower_bound > 0)))
        throw std::domain_error("alpha and lower_bound must be in (0, infty)");
    }

    static_assert(
        std::is_floating_point<_RealType>::value &&
            std::numeric_limits<_RealType>::is_iec559,
        "Class template rmolib::random::pareto_distribution<> must be "
        "parametrized with IEEE 759 conformant floating-point number");
  };

  pareto_distribution() { init_unit_uniform_real_distribution(); }

  explicit pareto_distribution(const _RealType alpha,
                               const _RealType lower_bound)
      : param_{alpha, lower_bound} {}
  pareto_distribution(const param_type& param) : param_{param} {
    init_unit_uniform_real_distribution();
  }

  // compiler generated ctor and assignment op is sufficient

  void reset() {}

  auto min() const { return lower_bound(); }
  auto max() const { return std::numeric_limits<result_type>::infinity(); }

  auto alpha() const { return param_.alpha(); }
  auto lower_bound() const { return param_.lower_bound(); }

  param_type param() const { return param_; }
  void param(const param_type& param) { param_ = param; }

  template <typename _EngineType>
  result_type operator()(_EngineType& engine) {
    return (*this)(engine, param_);
  }

  template <typename _EngineType>
  result_type operator()(_EngineType& engine, const param_type& param) {
    return param.lower_bound_ /
           std::pow(unit_uniform_real_distribution_(engine), 1. / param.alpha_);
  }

  friend bool operator==(const pareto_distribution& lhs,
                         const pareto_distribution& rhs) {
    return lhs.param_ == rhs.param_;
  }

  friend bool operator!=(const pareto_distribution& lhs,
                         const pareto_distribution& rhs) {
    return lhs.param_ != rhs.param_;
  }

 private:
  param_type param_{};
  _UnitUniformRealDistributionType unit_uniform_real_distribution_{};

  void init_unit_uniform_real_distribution() {
    if constexpr (std::is_constructible_v<_UnitUniformRealDistributionType,
                                          const _RealType, const _RealType>) {
      // TODO:
      // static_assert(is_distribution_type<_UnitUniformRealDistributionType>)
      using unit_param_type =
          typename _UnitUniformRealDistributionType::param_type;
      unit_uniform_real_distribution_.param(
          unit_param_type{_RealType{0}, _RealType{1}});
    }
  }

  static_assert(
      std::is_same<
          result_type,
          typename _UnitUniformRealDistributionType::result_type>::value,
      "Class template rmolib::random::pareto_distribution<> must be "
      "parametrized with unit_uniform_real_distribution-type with matching "
      "result_type");
};

/*
  // TODO: implement

  template <class _CharType, class _Traits, class _RealType>
  std::basic_ostream<_CharType, _Traits>&
  operator<<(std::basic_ostream<_CharType, _Traits>& os,
            pareto_distribution<_RealType>& dist);

  template <class _CharType, class _Traits, class _RealType>
  std::basic_istream<_CharType, _Traits>&
  operator>>(std::basic_istream<_CharType, _Traits>& is,
             pareto_distribution<_RealType>& dist);
*/

}  // namespace random

}  // namespace rmolib
