#pragma once

#include <algorithm>
#include <cmath>
#include <limits>
#include <type_traits>
#include <vector>

#include "rmolib/math/next_integral_value.hpp"
#include "rmolib/random/multivariate/internal/is_within.hpp"
#include "rmolib/random/multivariate/internal/mo_param_type.hpp"
#include "rmolib/random/univariate/exponential_distribution.hpp"
#include "rmolib/random/univariate/r_discrete_distribution.hpp"

namespace rmolib {

namespace random {

template <typename _Container,
          typename _UnitExponentialDistribution =
              exponential_distribution<typename _Container::value_type>,
          typename _DiscreteDistribution = r_discrete_distribution<
              typename _Container::size_type, typename _Container::value_type>>
class arnold_mo_distribution {
 public:
  using result_type = _Container;

  using value_type = typename _Container::value_type;
  using size_type = typename _Container::size_type;

  class param_type {
   public:
    using distribution_type = arnold_mo_distribution;

    param_type() = default;

    template <typename _ForwardIterator>
    explicit param_type(size_type dim, _ForwardIterator first,
                        _ForwardIterator last)
        : dim_{dim},
          total_intensity_{std::accumulate(first, last, value_type{0})},
          discrete_parm_{first, last} {
      __validate_input();
    }

    explicit param_type(size_type dim, _Container intensities)
        : param_type{dim, intensities.begin(), intensities.end()} {}

    param_type(size_type dim, std::initializer_list<value_type> wl)
        : param_type{dim, wl.begin(), wl.end()} {}

    // Used for construction from a different specialization
    template <
        typename _MOParamType,
        std::enable_if_t<!std::is_convertible_v<_MOParamType, param_type> &&
                             internal::is_mo_param_type_v<_MOParamType>,
                         int> = 0>
    explicit param_type(_MOParamType&& parm)
        : param_type{parm.dim(), parm.intensities()} {}

    // compiler generated ctor and assignment op is sufficient

    auto dim() const { return dim_; }
    auto intensities() const {
      auto out = discrete_parm_.probabilities();
      std::transform(out.cbegin(), out.cend(), out.begin(),
                     std::bind(std::multiplies<value_type>(),
                               std::placeholders::_1, total_intensity_));
      return out;
    }

    friend class arnold_mo_distribution;

    friend bool operator==(const param_type& lhs, const param_type& rhs) {
      return lhs.dim_ == rhs.dim_ &&
             lhs.total_intensity_ == rhs.total_intensity_ &&
             lhs.discrete_parm_ == rhs.discrete_parm_;
    }

    friend bool operator!=(const param_type& lhs, const param_type& rhs) {
      return !(lhs == rhs);
    }

   private:
    size_type dim_{1};
    value_type total_intensity_{1};
    typename _DiscreteDistribution::param_type discrete_parm_{{value_type{1}}};

    void __validate_input() const {
      if (!((size_type{1} << dim_) ==
            discrete_parm_.probabilities().size() + size_type{1}))
        throw std::domain_error("intensities vector has wrong size");
    }

    static_assert(
        std::is_floating_point_v<value_type>,
        "Class template rmolib::random::arnold_mo_distribution<> must be "
        "parametrized with floating point type");
  };

  arnold_mo_distribution() = default;

  template <typename _ForwardIterator>
  explicit arnold_mo_distribution(size_type dim, _ForwardIterator first,
                                  _ForwardIterator last)
      : parm_{dim, first, last} {
    init_unit_exponential_distribution();
  }

  explicit arnold_mo_distribution(size_type dim, _Container intensities)
      : parm_{dim, intensities} {}

  explicit arnold_mo_distribution(size_type dim,
                                  std::initializer_list<value_type>& wl)
      : parm_{dim, wl.begin(), wl.end()} {
    init_unit_exponential_distribution();
  }

  explicit arnold_mo_distribution(const param_type& parm) : parm_{parm} {
    init_unit_exponential_distribution();
  }

  // Used for construction from a different specialization
  template <typename _MOParamType,
            std::enable_if_t<
                !std::is_convertible_v<_MOParamType, arnold_mo_distribution> &&
                    !std::is_convertible_v<_MOParamType, param_type> &&
                    internal::is_mo_param_type_v<_MOParamType>,
                int> = 0>
  explicit arnold_mo_distribution(_MOParamType&& parm)
      : parm_{std::forward<_MOParamType>(parm)} {
    init_unit_exponential_distribution();
  }

  // compiler generated ctor and assignment op is sufficient

  void reset() {}

  auto min() const {
    result_type out(parm_.dim(), value_type{0});
    out.front() = value_type{-1};
    return out;
  }
  auto max() const {
    return result_type(dim(), std::numeric_limits<value_type>::infinity());
  }

  auto dim() const { return parm_.dim(); }
  auto intensities() const { return parm_.intensities(); }

  param_type param() const { return parm_; }
  void param(const param_type& parm) { parm_ = parm; }

  template <typename _EngineType>
  result_type operator()(_EngineType& engine) {
    return (*this)(engine, parm_);
  }

  template <typename _EngineType>
  result_type operator()(_EngineType& engine, const param_type& parm) {
    result_type out(parm.dim_);
    (*this)(engine, parm, out);
    return out;
  }

  template <typename _EngineType, typename _OutputContainer>
  void operator()(_EngineType& engine, const param_type& parm,
                  _OutputContainer& out) {
    // TODO: check compatibility
    using size_type = typename _Container::size_type;

    std::fill(out.begin(), out.end(), value_type{0});
    std::vector<bool> destroyed(parm.dim_, false);

    while (!std::all_of(destroyed.begin(), destroyed.end(),
                        [](bool v) { return v; })) {
      auto waiting_time =
          unit_exponential_distribution_(engine) / parm.total_intensity_;
      auto affected = math::next_integral_value(
          discrete_distribution_(engine, parm.discrete_parm_));

      for (size_type i = 0; i < parm.dim_; ++i) {
        if (!destroyed[i]) {
          out[i] += waiting_time;
          if (internal::is_within(i, affected)) destroyed[i] = true;
        }
      }
    }
  }

  friend bool operator==(const arnold_mo_distribution& lhs,
                         const arnold_mo_distribution& rhs) {
    return lhs.parm_ == rhs.parm_;
  }

  friend bool operator!=(const arnold_mo_distribution& lhs,
                         const arnold_mo_distribution& rhs) {
    return lhs.parm_ != rhs.parm_;
  }

 private:
  param_type parm_{};
  _UnitExponentialDistribution unit_exponential_distribution_{};
  _DiscreteDistribution discrete_distribution_{};

  void init_unit_exponential_distribution() {
    if constexpr (std::is_constructible_v<_UnitExponentialDistribution,
                                          const value_type>) {
      // static_assert(is_distribution_type<_UnitExponentialDistribution>)
      using unit_param_type = typename _UnitExponentialDistribution::param_type;
      unit_exponential_distribution_.param(unit_param_type{1.});
    }
  }

  static_assert(
      std::is_same_v<value_type,
                     typename _UnitExponentialDistribution::result_type>,
      "Class template rmolib::random::arnold_mo_distribution<> must be "
      "parametrized with unit_exponential_distribution-type with matching "
      "result_type");

  // TODO: check static_assert
  static_assert(
      std::is_same_v<size_type, typename _DiscreteDistribution::result_type>,
      "Class template rmolib::random::arnold_mo_distribution<> must be "
      "parametrized with discrete_distribution-type with matching "
      "size_type");
};

/*
  // TODO: implement

  template <class _CharType, class _Traits, typename _Container, typename
  _UnitExponentialDistribution> std::basic_ostream<_CharType, _Traits>&
  operator<<(std::basic_ostream<_CharType, _Traits>& os,
            arnold_mo_distribution<_Container, _UnitExponentialDistribution>&
  dist);

  template <class _CharType, class _Traits, typename _Container, typename
  _UnitExponentialDistribution> std::basic_istream<_CharType, _Traits>&
  operator>>(std::basic_istream<_CharType, _Traits>& is,
             arnold_mo_distribution<_Container, _UnitExponentialDistribution>&
  dist);
*/

}  // namespace random

}  // namespace rmolib
