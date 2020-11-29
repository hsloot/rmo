#pragma once

#include <cmath>
#include <limits>
#include <stdexcept>
#include <type_traits>

#include "rmolib/random/univariate/uniform_real_distribution.hpp"
#include "rmolib/type_traits/is_safe_numeric_cast.hpp"

namespace rmolib {

namespace random {

namespace internal {

template <typename _T, class = void>
struct __is_pareto_param_type : public std::false_type {};

template <typename _T>
struct __is_pareto_param_type<
    _T,
    std::enable_if_t<decltype(std::declval<_T>().alpha(), std::true_type())::
                         value&& decltype(std::declval<_T>().lower_bound(),
                                          std::true_type())::value>>
    : public std::true_type {};

}  // namespace internal

template <typename _T>
struct is_pareto_param_type
    : public internal::__is_pareto_param_type<std::remove_cv_t<_T>> {};

template <typename _T>
constexpr bool is_pareto_param_type_v = is_pareto_param_type<_T>::value;

template <typename _RealType, typename _UnitUniformRealDistribution =
                                  uniform_real_distribution<_RealType>>
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
    template <
        typename _ParetoParamType,
        std::enable_if_t<!std::is_convertible_v<_ParetoParamType, param_type> &&
                             is_pareto_param_type_v<_ParetoParamType>,
                         int> = 0>
    explicit param_type(_ParetoParamType&& parm)
        : param_type{parm.alpha(), parm.lower_bound()} {}

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
        std::is_floating_point_v<_RealType> &&
            std::numeric_limits<_RealType>::is_iec559,
        "Class template rmolib::random::pareto_distribution<> must be "
        "parametrized with IEEE 759 conformant floating-point number");
  };

  pareto_distribution() { init_unit_uniform_real_distribution(); }

  explicit pareto_distribution(const _RealType alpha,
                               const _RealType lower_bound)
      : parm_{alpha, lower_bound} {}

  explicit pareto_distribution(const param_type& param) : parm_{param} {
    init_unit_uniform_real_distribution();
  }

  // Used for construction from a different specialization
  template <typename _ParetoParamType,
            std::enable_if_t<
                !std::is_convertible_v<_ParetoParamType, pareto_distribution> &&
                    !std::is_convertible_v<_ParetoParamType, param_type> &&
                    is_pareto_param_type_v<_ParetoParamType>,
                int> = 0>
  explicit pareto_distribution(_ParetoParamType&& parm)
      : parm_{std::forward<_ParetoParamType>(parm)} {}

  // compiler generated ctor and assignment op is sufficient

  void reset() {}

  auto min() const { return lower_bound(); }
  auto max() const { return std::numeric_limits<result_type>::infinity(); }

  auto alpha() const { return parm_.alpha(); }
  auto lower_bound() const { return parm_.lower_bound(); }

  param_type param() const { return parm_; }
  void param(const param_type& param) { parm_ = param; }

  template <typename _EngineType>
  result_type operator()(_EngineType& engine) {
    return (*this)(engine, parm_);
  }

  template <typename _EngineType>
  result_type operator()(_EngineType& engine, const param_type& param) {
    return param.lower_bound_ /
           std::pow(unit_uniform_real_distribution_(engine), 1. / param.alpha_);
  }

  friend bool operator==(const pareto_distribution& lhs,
                         const pareto_distribution& rhs) {
    return lhs.parm_ == rhs.parm_;
  }

  friend bool operator!=(const pareto_distribution& lhs,
                         const pareto_distribution& rhs) {
    return lhs.parm_ != rhs.parm_;
  }

 private:
  param_type parm_{};
  _UnitUniformRealDistribution unit_uniform_real_distribution_{};

  void init_unit_uniform_real_distribution() {
    if constexpr (std::is_constructible_v<_UnitUniformRealDistribution,
                                          const _RealType, const _RealType>) {
      using unit_dist_t = _UnitUniformRealDistribution;
      using unit_parm_t = typename unit_dist_t::param_type;
      using std::swap;

      auto tmp = unit_dist_t{unit_parm_t{_RealType{0}, _RealType{1}}};
      swap(unit_uniform_real_distribution_, tmp);
    }
  }

  static_assert(
      type_traits::is_safe_numeric_cast_v<
          result_type, typename _UnitUniformRealDistribution::result_type>,
      "Class template rmolib::random::pareto_distribution<> must be "
      "parametrized with unit_uniform_real_distribution-type convertible to "
      "result_type");
};

/*
  // TODO: implement

  template <class _CharType, class _Traits, typename _RealType, typename
  _UnitUniformRealDistribution> std::basic_ostream<_CharType, _Traits>&
  operator<<(std::basic_ostream<_CharType, _Traits>& os,
            pareto_distribution<_RealType, _UnitUniformRealDistribution>& dist);

  template <class _CharType, class _Traits, typename _RealType, typename
  _UnitUniformRealDistribution> std::basic_istream<_CharType, _Traits>&
  operator>>(std::basic_istream<_CharType, _Traits>& is,
             pareto_distribution<_RealType, _UnitUniformRealDistribution>&
  dist);
*/

}  // namespace random

}  // namespace rmolib
