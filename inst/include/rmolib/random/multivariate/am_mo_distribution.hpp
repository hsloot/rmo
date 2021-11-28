#pragma once

#include <algorithm>
#include <cmath>
#include <limits>
#include <numeric>
#include <stdexcept>
#include <type_traits>
#include <vector>

#include "rmolib/bit/bit_fill.hpp"
#include "rmolib/math/next_integral_value.hpp"
#include "rmolib/random/multivariate/internal/is_within.hpp"
#include "rmolib/random/multivariate/internal/mo_param_type.hpp"
#include "rmolib/type_traits/is_safe_numeric_cast.hpp"

namespace rmolib {

namespace random {

template <typename _RealType, typename _ExponentialDistribution,
          typename _DiscreteDistribution>
class am_mo_distribution {
 public:
  using result_type = std::vector<_RealType>;

  class param_type {
   public:
    using distribution_type = am_mo_distribution;

    param_type() { __init_empty(); }

    template <typename _InputIterator>
    explicit param_type(const std::size_t dim, _InputIterator first,
                        _InputIterator last)
        : dim_{dim} {
      __init(first, last);
    }

    template <typename _Container>
    explicit param_type(const std::size_t dim, const _Container& intensities)
        : param_type{dim, intensities.cbegin(), intensities.cend()} {}

    // Used for construction from a different specialization
    template <
        typename _MOParamType,
        std::enable_if_t<!std::is_convertible_v<_MOParamType, param_type> &&
                             is_mo_param_type_v<_MOParamType>,
                         int> = 0>
    explicit param_type(_MOParamType&& parm)
        : param_type{parm.dim(), parm.intensities()} {}

    // compiler generated ctor and assignment op is sufficient

    auto dim() const { return dim_; }
    auto intensities() const {
      const auto& [poisson_parm, discrete_parm] = compound_poisson_parm_;
      auto out = discrete_parm.probabilities();
      std::transform(out.cbegin(), out.cend(), out.begin(),
                     [total_intensity = poisson_parm.lambda()](const auto v) {
                       return v * total_intensity;
                     });
      return out;
    }

    friend class am_mo_distribution;

    friend bool operator==(const param_type& lhs, const param_type& rhs) {
      return lhs.dim_ == rhs.dim_ &&
             lhs.compound_poisson_parm_ == rhs.compound_poisson_parm_;
    }

    friend bool operator!=(const param_type& lhs, const param_type& rhs) {
      return !(lhs == rhs);
    }

   private:
    using exponential_parm_t = typename _ExponentialDistribution::param_type;
    using discrete_parm_t = typename _DiscreteDistribution::param_type;
    using compound_poisson_parm_t =
        std::pair<exponential_parm_t, discrete_parm_t>;

    std::size_t dim_{1};
    compound_poisson_parm_t compound_poisson_parm_{};

    template <typename _ForwardIterator>
    void __validate_input(const std::size_t dim, _ForwardIterator first,
                          _ForwardIterator last) const {
      using std::distance;

      if (!(bit::bit_fill<std::size_t>(0, dim, true) == distance(first, last)))
        throw std::domain_error("intensities vector has wrong size");

      if (!(std::accumulate(first, last, _RealType{0}) > 0))
        throw std::domain_error("sum of intensities must be positive");
    }

    void __init_empty() {
      compound_poisson_parm_ = std::make_pair(exponential_parm_t{_RealType{1}},
                                              discrete_parm_t{_RealType{1}});
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
      __init(tmp.cbegin(), tmp.cend());
    }

    template <typename _ForwardIterator>
    void __init(_ForwardIterator first, _ForwardIterator last,
                std::forward_iterator_tag) {
      __validate_input(dim_, first, last);
      auto poisson_parm =
          exponential_parm_t{std::accumulate(first, last, _RealType{0})};
      auto discrete_parm = discrete_parm_t{first, last};
      compound_poisson_parm_ =
          std::make_pair(std::move(poisson_parm), std::move(discrete_parm));
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
        "Class template rmolib::random::am_mo_distribution<> must be "
        "parametrized with floating point type");
  };

  am_mo_distribution() = default;

  template <typename _ForwardIterator>
  explicit am_mo_distribution(const std::size_t dim, _ForwardIterator first,
                                  _ForwardIterator last)
      : parm_{dim, first, last} {}

  template <typename _Container>
  explicit am_mo_distribution(const std::size_t dim,
                                  const _Container& intensities)
      : parm_{dim, intensities} {}

  explicit am_mo_distribution(const param_type& parm) : parm_{parm} {}

  // Used for construction from a different specialization
  template <typename _MOParamType,
            std::enable_if_t<
                !std::is_convertible_v<_MOParamType, am_mo_distribution> &&
                    !std::is_convertible_v<_MOParamType, param_type> &&
                    is_mo_param_type_v<_MOParamType>,
                int> = 0>
  explicit am_mo_distribution(_MOParamType&& parm)
      : parm_{std::forward<_MOParamType>(parm)} {}

  // compiler generated ctor and assignment op is sufficient

  void reset() {}

  auto min() const { return result_type(dim(), _RealType{0}); }
  auto max() const {
    return result_type(dim(), std::numeric_limits<_RealType>::infinity());
  }

  auto dim() const { return parm_.dim(); }
  auto intensities() const { return parm_.intensities(); }

  param_type param() const { return parm_; }
  void param(const param_type& parm) { parm_ = parm; }

  template <typename _Engine>
  result_type operator()(_Engine&& engine) {
    return (*this)(std::forward<_Engine>(engine), parm_);
  }

  template <typename _Engine>
  result_type operator()(_Engine&& engine, const param_type& parm) {
    result_type out(parm.dim_);
    (*this)(std::forward<_Engine>(engine), parm, out);
    return out;
  }

  //! implicitely assumes that `out.size() == dim`
  template <typename _Engine, typename _Container>
  void operator()(_Engine&& engine, const param_type& parm, _Container& out) {
    // TODO: check compatibility
    const auto dim = out.size();
    const auto all = bit::bit_fill<std::size_t>(0, dim, true);
    auto state = std::make_pair(_RealType{0}, std::size_t{0});
    auto& [time, dead] = state;
    while (dead != all) {
      std::size_t alive_before_transition = ~dead & all;
      state = __compound_poisson_process(engine, parm.compound_poisson_parm_,
                                         state);

      for (std::size_t i = 0; i < dim; ++i)
        if (internal::is_within(i, alive_before_transition)) out[i] = time;
    }
  }

  friend bool operator==(const am_mo_distribution& lhs,
                         const am_mo_distribution& rhs) {
    return lhs.parm_ == rhs.parm_;
  }

  friend bool operator!=(const am_mo_distribution& lhs,
                         const am_mo_distribution& rhs) {
    return !(lhs == rhs);
  }

 private:
  using compound_poisson_parm_t = typename param_type::compound_poisson_parm_t;

  param_type parm_{};
  _ExponentialDistribution exponential_dist_{};
  _DiscreteDistribution discrete_dist_{};

  template <typename _Engine>
  auto __compound_poisson_process(_Engine&& engine,
                                  const compound_poisson_parm_t& parm,
                                  std::pair<_RealType, std::size_t> state) {
    const auto& [poisson_parm, discrete_parm] = parm;
    auto& [time, location] = state;
    time += exponential_dist_(engine, poisson_parm);
    location |=
        math::next_integral_value(discrete_dist_(engine, discrete_parm));
    return state;
  }

  static_assert(
      type_traits::is_safe_numeric_cast_v<
          _RealType, typename _ExponentialDistribution::result_type>,
      "Class template rmolib::random::am_mo_distribution<> must be "
      "parametrized with unit_exponential_distribution-type with suitable "
      "result_type");

  static_assert(
      type_traits::is_safe_numeric_cast_v<
          std::size_t, typename _DiscreteDistribution::result_type>,
      "Class template rmolib::random::am_mo_distribution<> must be "
      "parametrized with discrete_distribution-type with suitable "
      "result_type");
};

/*
  // TODO: implement

  template <class _CharType, class _Traits, typename _RealType, typename
  _UnitExponentialDistribution> std::basic_ostream<_CharType, _Traits>&
  operator<<(std::basic_ostream<_CharType, _Traits>& os,
            am_mo_distribution<_RealType, _UnitExponentialDistribution>&
  dist);

  template <class _CharType, class _Traits, typename _RealType, typename
  _UnitExponentialDistribution> std::basic_istream<_CharType, _Traits>&
  operator>>(std::basic_istream<_CharType, _Traits>& is,
             am_mo_distribution<_RealType, _UnitExponentialDistribution>&
  dist);
*/

}  // namespace random

}  // namespace rmolib
