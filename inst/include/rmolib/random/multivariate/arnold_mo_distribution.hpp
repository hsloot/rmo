#pragma once

#include <algorithm>
#include <cmath>
#include <limits>
#include <stdexcept>
#include <type_traits>
#include <vector>

#include "rmolib/bit/bit_fill.hpp"
#include "rmolib/math/next_integral_value.hpp"
#include "rmolib/random/multivariate/internal/is_within.hpp"
#include "rmolib/random/multivariate/internal/mo_param_type.hpp"
#include "rmolib/random/univariate/exponential_distribution.hpp"
#include "rmolib/random/univariate/r_discrete_distribution.hpp"
#include "rmolib/type_traits/is_safe_numeric_cast.hpp"

namespace rmolib {

namespace random {

template <typename _RealType,
          typename _ExponentialDistribution =
              exponential_distribution<_RealType>,
          typename _DiscreteDistribution =
              r_discrete_distribution<std::size_t, _RealType>>
class arnold_mo_distribution {
 public:
  using result_type = std::vector<_RealType>;

  class param_type {
   public:
    using distribution_type = arnold_mo_distribution;

    param_type() { __init_empty(); }

    template <typename _ForwardIterator>
    explicit param_type(const std::size_t dim, _ForwardIterator first,
                        _ForwardIterator last)
        : dim_{dim} {
      __init(first, last);
    }

    template <typename _Container>
    explicit param_type(const std::size_t dim, const _Container& intensities)
        : param_type{dim, intensities.begin(), intensities.end()} {}

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
                     [total_intensity = poisson_parm_.lambda()](auto v) {
                       return v * total_intensity;
                     });
      return out;
    }

    friend class arnold_mo_distribution;

    friend bool operator==(const param_type& lhs, const param_type& rhs) {
      return lhs.dim_ == rhs.dim_ && lhs.poisson_parm_ == rhs.poisson_parm_ &&
             lhs.discrete_parm_ == rhs.discrete_parm_;
    }

    friend bool operator!=(const param_type& lhs, const param_type& rhs) {
      return !(lhs == rhs);
    }

   private:
    using exponential_parm_t = typename _ExponentialDistribution::param_type;
    using discrete_parm_t = typename _DiscreteDistribution::param_type;

    std::size_t dim_{1};
    exponential_parm_t poisson_parm_{};
    discrete_parm_t discrete_parm_{};

    template <typename _ForwardIterator>
    void __validate_input(const std::size_t dim, _ForwardIterator first,
                          _ForwardIterator last) const {
      using std::distance;

      if (!(bit::bit_fill<std::size_t>(0, dim, true) == distance(first, last)))
        throw std::domain_error("intensities vector has wrong size");

      if (!(std::accumulate(first, last, _RealType{0}) > 0))
        throw std::domain_error("Poisson intensity must be positive");
    }

    void __init_empty() {
      poisson_parm_ = exponential_parm_t{_RealType{1}};
      discrete_parm_ = {_RealType{1}};
    }

    template <typename _InputIterator>
    void __init_empty(_InputIterator first, _InputIterator last) {
      __validate_input(dim_, first, last);
      __init_empty();
    }

    template <typename _InputIterator>
    void __init(_InputIterator first, _InputIterator last,
                std::input_iterator_tag) {
      std::vector<_RealType> tmp{first, last};
      __init(tmp.begin(), tmp.end());
    }

    template <typename _ForwardIterator>
    void __init(_ForwardIterator first, _ForwardIterator last,
                std::forward_iterator_tag) {
      __validate_input(dim_, first, last);
      poisson_parm_ =
          exponential_parm_t{std::accumulate(first, last, _RealType{0})};
      discrete_parm_ = discrete_parm_t{first, last};
    }

    template <typename _InputIterator>
    void __init(_InputIterator first, _InputIterator last) {
      if (first == last) {
        __init_empty(first, last);
      } else {
        using std::iterator_traits;
        using iterator_tag =
            typename iterator_traits<_InputIterator>::iterator_category;
        __init(first, last, iterator_tag{});
      }
    }

    static_assert(
        std::is_floating_point_v<_RealType>,
        "Class template rmolib::random::arnold_mo_distribution<> must be "
        "parametrized with floating point type");
  };

  arnold_mo_distribution() = default;

  template <typename _ForwardIterator>
  explicit arnold_mo_distribution(const std::size_t dim, _ForwardIterator first,
                                  _ForwardIterator last)
      : parm_{dim, first, last} {}

  template <typename _Container>
  explicit arnold_mo_distribution(const std::size_t dim,
                                  const _Container& intensities)
      : parm_{dim, intensities} {}

  explicit arnold_mo_distribution(const param_type& parm) : parm_{parm} {}

  // Used for construction from a different specialization
  template <typename _MOParamType,
            std::enable_if_t<
                !std::is_convertible_v<_MOParamType, arnold_mo_distribution> &&
                    !std::is_convertible_v<_MOParamType, param_type> &&
                    internal::is_mo_param_type_v<_MOParamType>,
                int> = 0>
  explicit arnold_mo_distribution(_MOParamType&& parm)
      : parm_{std::forward<_MOParamType>(parm)} {}

  // compiler generated ctor and assignment op is sufficient

  void reset() {}

  auto min() const { return result_type(dim(), _RealType{-1}); }
  auto max() const {
    return result_type(dim(), std::numeric_limits<_RealType>::infinity());
  }

  auto dim() const { return parm_.dim(); }
  auto intensities() const { return parm_.intensities(); }

  param_type param() const { return parm_; }
  void param(const param_type& parm) { parm_ = parm; }

  template <typename _Engine>
  result_type operator()(_Engine& engine) {
    return (*this)(engine, parm_);
  }

  template <typename _Engine>
  result_type operator()(_Engine& engine, const param_type& parm) {
    result_type out(parm.dim_);
    (*this)(engine, parm, out);
    return out;
  }

  template <typename _Engine, typename _Container>
  void operator()(_Engine& engine, const param_type& parm, _Container& out) {
    // TODO: check compatibility
    const auto dim = out.size();
    const auto all = bit::bit_fill<std::size_t>(0, dim, true);
    auto state = std::make_pair(_RealType{0}, std::size_t{0});
    auto& [time, dead] = state;
    while (dead != all) {
      std::size_t alive_before_transition = ~dead & all;
      state = __poisson_process(engine, parm, state);

      for (std::size_t i = 0; i < dim; ++i)
        if (internal::is_within(i, alive_before_transition)) out[i] = time;
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
  _ExponentialDistribution exponential_dist_{};
  _DiscreteDistribution discrete_dist_{};

  template <typename _Engine>
  auto __poisson_process(_Engine& engine, const param_type& parm,
                         std::pair<_RealType, std::size_t> state) {
    auto& [time, location] = state;
    time += exponential_dist_(engine, parm.poisson_parm_);
    location |=
        math::next_integral_value(discrete_dist_(engine, parm.discrete_parm_));
    return state;
  }

  static_assert(
      type_traits::is_safe_numeric_cast_v<
          _RealType, typename _ExponentialDistribution::result_type>,
      "Class template rmolib::random::arnold_mo_distribution<> must be "
      "parametrized with unit_exponential_distribution-type with suitable "
      "result_type");

  // TODO: check static_assert
  static_assert(
      type_traits::is_safe_numeric_cast_v<
          std::size_t, typename _DiscreteDistribution::result_type>,
      "Class template rmolib::random::arnold_mo_distribution<> must be "
      "parametrized with discrete_distribution-type with suitable "
      "result_type");
};

/*
  // TODO: implement

  template <class _CharType, class _Traits, typename _RealType, typename
  _UnitExponentialDistribution> std::basic_ostream<_CharType, _Traits>&
  operator<<(std::basic_ostream<_CharType, _Traits>& os,
            arnold_mo_distribution<_RealType, _UnitExponentialDistribution>&
  dist);

  template <class _CharType, class _Traits, typename _RealType, typename
  _UnitExponentialDistribution> std::basic_istream<_CharType, _Traits>&
  operator>>(std::basic_istream<_CharType, _Traits>& is,
             arnold_mo_distribution<_RealType, _UnitExponentialDistribution>&
  dist);
*/

}  // namespace random

}  // namespace rmolib
