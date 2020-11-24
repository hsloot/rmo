#pragma once

#include <algorithm>
#include <cmath>
#include <limits>
#include <type_traits>
#include <vector>

#include "rmolib/algorithm/shuffle.hpp"
#include "rmolib/math/binomial_coefficient.hpp"
#include "rmolib/math/next_integral_value.hpp"
#include "rmolib/random/multivariate/internal/exmo_param_type.hpp"
#include "rmolib/random/univariate/exponential_distribution.hpp"
#include "rmolib/random/univariate/r_discrete_distribution.hpp"
#include "rmolib/random/univariate/uniform_int_distribution.hpp"

namespace rmolib {

namespace random {

template <typename _Container,
          typename _ExponentialDistribution =
              exponential_distribution<typename _Container::value_type>,
          typename _UniformIntDistribution =
              uniform_int_distribution<typename _Container::size_type>,
          typename _DiscreteDistribution = r_discrete_distribution<
              typename _Container::size_type, typename _Container::value_type>>
class markovian_exmo_distribution {
 public:
  using result_type = _Container;

  using value_type = typename _Container::value_type;
  using size_type = typename _Container::size_type;

  class param_type {
   public:
    using distribution_type = markovian_exmo_distribution;

    param_type() = default;

    template <typename _ForwardIterator>
    explicit param_type(size_type dim, _ForwardIterator first,
                        _ForwardIterator last)
        : dim_{dim}, total_intensities_parm_(dim_), discrete_parms_(dim_) {
      auto next_submodel = [](auto& v) {
        std::transform(v.cbegin() + 1, v.cend(), v.cbegin(), v.begin(),
                       std::plus<double>());
        v.pop_back();
      };
      auto scale_ex_intensities = [](auto v) {
        std::transform(v.cbegin(), v.cend(), v.begin(),
                       [j = std::size_t{0}, d = v.size()](double x) mutable {
                         return math::multiply_binomial_coefficient(x, d, ++j);
                       });
        return v;
      };

      std::vector<value_type> ex_intensities{first, last};
      for (std::size_t i = 0; i < total_intensities_parm_.size(); ++i) {
        auto scaled_ex_intensities = scale_ex_intensities(ex_intensities);

        total_intensities_parm_[i] = exponential_param_t{
            std::accumulate(scaled_ex_intensities.cbegin(),
                            scaled_ex_intensities.cend(), value_type{0})};
        discrete_parms_[i] = discrete_param_t{scaled_ex_intensities};
        next_submodel(ex_intensities);
      }
    }

    explicit param_type(size_type dim, _Container ex_intensities)
        : param_type{dim, ex_intensities.begin(), ex_intensities.end()} {}

    param_type(size_type dim, std::initializer_list<value_type> wl)
        : param_type{dim, wl.begin(), wl.end()} {}

    // Used for construction from a different specialization
    template <
        typename _ExMOParamType,
        std::enable_if_t<!std::is_convertible_v<_ExMOParamType, param_type> &&
                             internal::is_exmo_param_type_v<_ExMOParamType>,
                         int> = 0>
    explicit param_type(_ExMOParamType&& parm)
        : param_type{parm.dim(), parm.ex_intensities()} {}

    // compiler generated ctor and assignment op is sufficient

    auto dim() const { return dim_; }
    auto ex_intensities() const {
      auto total_intensity = total_intensities_parm_[0].lambda();
      auto out = discrete_parms_[0].probabilities();
      std::transform(out.cbegin(), out.cend(), out.begin(),
                     std::bind(std::multiplies<value_type>{},
                               std::placeholders::_1, total_intensity));
      std::transform(out.cbegin(), out.cend(), out.begin(),
                     [j = std::size_t{0}, d = out.size()](auto v) mutable {
                       return math::multiply_binomial_coefficient(
                           v, d, ++j, std::divides<decltype(v)>{});
                     });
      return out;
    }

    friend class markovian_exmo_distribution;

    friend bool operator==(const param_type& lhs, const param_type& rhs) {
      return lhs.dim_ == rhs.dim_ &&
             lhs.total_intensities_parm_ == rhs.total_intensities_parm_ &&
             lhs.discrete_parms_ == rhs.discrete_parms_;
    }

    friend bool operator!=(const param_type& lhs, const param_type& rhs) {
      return !(lhs == rhs);
    }

   private:
    typedef typename _ExponentialDistribution::param_type exponential_param_t;
    using discrete_param_t = typename _DiscreteDistribution::param_type;

    size_type dim_{1};
    std::vector<exponential_param_t> total_intensities_parm_{
        {exponential_param_t{}}};
    std::vector<discrete_param_t> discrete_parms_{{discrete_param_t{}}};

    static_assert(
        std::is_floating_point_v<value_type>,
        "Class template rmolib::random::markovian_exmo_distribution<> must be "
        "parametrized with floating point type");
  };

  markovian_exmo_distribution() = default;

  template <typename _ForwardIterator>
  explicit markovian_exmo_distribution(size_type dim, _ForwardIterator first,
                                       _ForwardIterator last)
      : parm_{dim, first, last} {}

  explicit markovian_exmo_distribution(size_type dim, _Container ex_intensities)
      : parm_{dim, ex_intensities} {}

  explicit markovian_exmo_distribution(size_type dim,
                                       std::initializer_list<value_type>& wl)
      : parm_{dim, wl.begin(), wl.end()} {}

  explicit markovian_exmo_distribution(const param_type& parm) : parm_{parm} {}

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
  auto ex_intensities() const { return parm_.ex_intensities(); }

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
    std::fill(out.begin(), out.end(), 0);
    size_type state{0};
    while (state < parm.dim_) {
      auto waiting_time = exponential_distribution_(
          engine, parm.total_intensities_parm_[state]);
      for (size_type i = state; i < parm.dim_; ++i) out[i] += waiting_time;
      state += math::next_integral_value(
          discrete_distribution_(engine, parm.discrete_parms_[state]));
    }
    auto shuffle = [&uniform_int_distribution_=uniform_int_distribution_,&engine](auto& out) {
      std::vector<std::remove_reference_t<decltype(out[0])>> cpy{out.begin(), out.end()};
      std::vector<std::size_t> perm(cpy.size());
      std::iota(perm.begin(), perm.end(), 0);
      algorithm::shuffle(perm.begin(), perm.end(), engine, uniform_int_distribution_);
      // for r comparability
      std::reverse(perm.begin(), perm.end());
      for (auto i=0; i<cpy.size(); ++i)
        out[i] = cpy[perm[i]];
    };
    shuffle(out);
  }

  friend bool operator==(const markovian_exmo_distribution& lhs,
                         const markovian_exmo_distribution& rhs) {
    return lhs.parm_ == rhs.parm_;
  }

  friend bool operator!=(const markovian_exmo_distribution& lhs,
                         const markovian_exmo_distribution& rhs) {
    return lhs.parm_ != rhs.parm_;
  }

 private:
  param_type parm_{};
  _UniformIntDistribution uniform_int_distribution_{};
  _ExponentialDistribution exponential_distribution_{};
  _DiscreteDistribution discrete_distribution_{};

  static_assert(
      std::is_same<value_type,
                   typename _ExponentialDistribution::result_type>::value,
      "Class template rmolib::random::markovian_exmo_distribution<> must be "
      "parametrized with exponential_distribution-type with matching "
      "result_type");

  // TODO: check static_assert
  static_assert(
      std::is_same<size_type,
                   typename _DiscreteDistribution::result_type>::value,
      "Class template rmolib::random::markovian_exmo_distribution<> must be "
      "parametrized with discrete_distribution-type with matching "
      "size_type");
};

/*
  // TODO: implement

  template <class _CharType, class _Traits, typename _Container, typename
  _ExponentialDistribution> std::basic_ostream<_CharType, _Traits>&
  operator<<(std::basic_ostream<_CharType, _Traits>& os,
            markovian_exmo_distribution<_Container,
  _ExponentialDistribution>& dist);

  template <class _CharType, class _Traits, typename _Container, typename
  _ExponentialDistribution> std::basic_istream<_CharType, _Traits>&
  operator>>(std::basic_istream<_CharType, _Traits>& is,
             markovian_exmo_distribution<_Container,
  _ExponentialDistribution>& dist);
*/

}  // namespace random

}  // namespace rmolib
