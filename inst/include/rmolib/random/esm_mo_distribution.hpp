#pragma once

#include <cmath>
#include <limits>
#include <type_traits>
#include <vector>

#include "rmolib/random/exponential_distribution.hpp"
#include "rmolib/random/internal/is_within.hpp"
#include "rmolib/random/internal/mo_param_type.hpp"

namespace rmolib {

namespace random {

template <typename _T>
using is_mo_param_type = internal::is_mo_param_type<_T>;

template <typename _T>
constexpr bool is_mo_param_type_v = internal::is_mo_param_type_v<_T>;

template <typename _Container,
          typename _UnitExponentialDistribution =
              exponential_distribution<typename _Container::value_type>>
class esm_mo_distribution {
 public:
  using result_type = _Container;

  using value_type = typename _Container::value_type;
  using size_type = typename _Container::size_type;

  class param_type {
   public:
    using distribution_type = esm_mo_distribution;

    param_type() = default;

    template <typename _InputIterator>
    explicit param_type(size_type dim, _InputIterator first,
                        _InputIterator last)
        : dim_{dim}, intensities_{first, last} {
      __validate_input();
    }

    explicit param_type(size_type dim, _Container intensities)
        : dim_{dim}, intensities_{intensities} {
      __validate_input();
    }

    param_type(size_type dim, std::initializer_list<value_type> wl)
        : param_type{dim, wl.begin(), wl.end()} {}

    // Used for construction from a different specialization
    template <typename _MOParamType,
              typename std::enable_if<
                  !std::is_convertible_v<_MOParamType, param_type> &&
                      is_mo_param_type_v<_MOParamType>,
                  int>::type = 0>
    explicit param_type(_MOParamType&& parm)
        : param_type{parm.dim(), parm.intensities()} {}

    // compiler generated ctor and assignment op is sufficient

    auto dim() const { return dim_; }

    auto intensities() const { return intensities_; }

    friend class esm_mo_distribution;

    friend bool operator==(const param_type& lhs, const param_type& rhs) {
      return lhs.dim_ == rhs.dim_ && lhs.intensities_ == rhs.intensities_;
    }

    friend bool operator!=(const param_type& lhs, const param_type& rhs) {
      return !(lhs == rhs);
    }

   private:
    size_type dim_{1};
    _Container intensities_{{value_type{1}}};

    void __validate_input() const {
      if (!((size_type{1} << dim_) == intensities_.size() + size_type{1}))
        throw std::domain_error("intensities vector has wrongsize");

      if (!std::all_of(intensities_.begin(), intensities_.end(),
                       [](const auto v) { return v >= 0; }))
        throw std::domain_error("negative intensities not allowed");
    }

    static_assert(
        std::is_floating_point<value_type>::value,
        "Class template rmolib::random::esm_mo_distribution<> must be "
        "parametrized with floating point type");
  };

  esm_mo_distribution() = default;

  template <typename _InputIterator>
  explicit esm_mo_distribution(size_type dim, _InputIterator first,
                               _InputIterator last)
      : parm_{dim, first, last} {
    init_unit_exponential_distribution();
  }

  explicit esm_mo_distribution(size_type dim, _Container intensities)
      : parm_{dim, intensities} {}

  explicit esm_mo_distribution(size_type dim,
                               std::initializer_list<value_type>& wl)
      : parm_{dim, wl.begin(), wl.end()} {
    init_unit_exponential_distribution();
  }

  explicit esm_mo_distribution(const param_type& parm) : parm_{parm} {
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
    // TODO: static_assert if _OutputContainer::value_type is compatible
    using size_type = typename _Container::size_type;

    std::fill(out.begin(), out.end(), std::numeric_limits<value_type>::infinity());
    for (size_type j = 0, num_shocks = parm.intensities_.size(); j < num_shocks;
         ++j) {
      if (parm.intensities_[j] > 0) {
        auto shock_time =
            unit_exponential_distribution_(engine) / parm.intensities_[j];
        for (size_type i = 0; i < parm.dim_; ++i) {
          if (internal::is_within(i, j))
            out[i] = std::min(out[i], shock_time);
        }
      }
    }
  }

  friend bool operator==(const esm_mo_distribution& lhs,
                         const esm_mo_distribution& rhs) {
    return lhs.parm_ == rhs.parm_;
  }

  friend bool operator!=(const esm_mo_distribution& lhs,
                         const esm_mo_distribution& rhs) {
    return lhs.parm_ != rhs.parm_;
  }

 private:
  param_type parm_{};
  _UnitExponentialDistribution unit_exponential_distribution_{};

  void init_unit_exponential_distribution() {
    if constexpr (std::is_constructible_v<_UnitExponentialDistribution,
                                          const value_type>) {
      // static_assert(is_distribution_type<_UnitExponentialDistribution>)
      using unit_param_type = typename _UnitExponentialDistribution::param_type;
      unit_exponential_distribution_.param(unit_param_type{1.});
    }
  }

  static_assert(
      std::is_same<value_type,
                   typename _UnitExponentialDistribution::result_type>::value,
      "Class template rmolib::random::esm_mo_distribution<> must be "
      "parametrized with unit_exponential_distribution-type with matching "
      "result_type");
};

/*
  // TODO: implement

  template <class _CharType, class _Traits, typename _Container, typename
  _UnitExponentialDistribution> std::basic_ostream<_CharType, _Traits>&
  operator<<(std::basic_ostream<_CharType, _Traits>& os,
            esm_mo_distribution<_Container, _UnitExponentialDistribution>&
  dist);

  template <class _CharType, class _Traits, typename _Container, typename
  _UnitExponentialDistribution> std::basic_istream<_CharType, _Traits>&
  operator>>(std::basic_istream<_CharType, _Traits>& is,
             esm_mo_distribution<_Container, _UnitExponentialDistribution>&
  dist);
*/

}  // namespace random

}  // namespace rmolib
