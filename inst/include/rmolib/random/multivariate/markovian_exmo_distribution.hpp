#pragma once

#include <algorithm>
#include <cmath>
#include <limits>
#include <type_traits>
#include <vector>
#include <numeric>

#include "rmolib/math/binomial_coefficient.hpp"
#include "rmolib/math/next_integral_value.hpp"
#include "rmolib/random/multivariate/internal/exmo_param_type.hpp"
#include "rmolib/type_traits/iterator.hpp"
#include "rmolib/type_traits/is_safe_numeric_cast.hpp"

namespace rmolib {

namespace random {

template <typename _RealType, typename _ExponentialDistribution,
          typename _UniformIntDistribution, typename _DiscreteDistribution, typename _ShuffleOperation>
class markovian_exmo_distribution {
 public:
  using result_type = std::vector<_RealType>;

  class param_type {
   public:
    using distribution_type = markovian_exmo_distribution;

    param_type() = default;

    template <typename _InputIterator>
    explicit param_type(const std::size_t dim, _InputIterator first,
                        _InputIterator last)
        : dim_{dim} {
      __init(first, last);
    }

    template <typename _Container>
    explicit param_type(const std::size_t dim, const _Container& ex_intensities)
        : param_type{dim, ex_intensities.cbegin(), ex_intensities.cend()} {}

    // Used for construction from a different specialization
    template <
        typename _ExMOParamType,
        std::enable_if_t<!std::is_convertible_v<_ExMOParamType, param_type> &&
                             is_exmo_param_type_v<_ExMOParamType>,
                         int> = 0>
    explicit param_type(_ExMOParamType&& parm)
        : param_type{parm.dim(), parm.ex_intensities()} {}

    // compiler generated ctor and assignment op is sufficient

    auto dim() const { return dim_; }
    auto ex_intensities() const {
      const auto& [intensity_parm, discrete_parm] = markov_parm_.front();
      auto out = discrete_parm.probabilities();
      std::transform(out.cbegin(), out.cend(), out.begin(),
                     [total_intensity = intensity_parm.lambda()](const auto v) {
                       return v * total_intensity;
                     });
      std::transform(
          out.cbegin(), out.cend(), out.begin(),
          [j = std::size_t{0}, d = out.size()](const auto v) mutable {
            return math::multiply_binomial_coefficient(
                v, d, ++j, std::divides<std::remove_cv_t<decltype(v)>>{});
          });
      return out;
    }

    friend class markovian_exmo_distribution;

    friend bool operator==(const param_type& lhs, const param_type& rhs) {
      return lhs.dim_ == rhs.dim_ && lhs.markov_parm_ == rhs.markov_parm_;
    }

    friend bool operator!=(const param_type& lhs, const param_type& rhs) {
      return !(lhs == rhs);
    }

   private:
    using exponential_parm_t = typename _ExponentialDistribution::param_type;
    using discrete_parm_t = typename _DiscreteDistribution::param_type;
    using markov_parm_t =
        std::vector<std::pair<exponential_parm_t, discrete_parm_t>>;

    std::size_t dim_{1};
    std::vector<std::pair<exponential_parm_t, discrete_parm_t>> markov_parm_{};

    template <typename _ForwardIterator>
    void __validate_input(const std::size_t dim, _ForwardIterator first,
                          _ForwardIterator last) const {
      using std::distance;

      if (!(dim == distance(first, last)))
        throw std::domain_error("ex_intensities vector has wrong length");
    }

    void __init_empty() {
      markov_parm_ = {std::make_pair(exponential_parm_t{}, discrete_parm_t{})};
    }

    template <typename _ForwardIterator>
    void __init_empty(_ForwardIterator first, _ForwardIterator last) {
      __validate_input(dim_, first, last);
      __init_empty();
    }

    template <typename _InputIterator>
    void __init(_InputIterator first, _InputIterator last,
                std::input_iterator_tag) {
      std::vector<_RealType> tmp(first, last);
      __init(tmp.cbegin(), tmp.cend());
    }

    template <typename _InputIterator>
    void __init(_InputIterator first, _InputIterator last,
                std::forward_iterator_tag) {
      __validate_input(dim_, first, last);

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

      markov_parm_.clear();
      std::vector<_RealType> ex_intensities{first, last};
      while (!ex_intensities.empty()) {
        auto scaled_ex_intensities = scale_ex_intensities(ex_intensities);
        auto intensity_parm = exponential_parm_t{
            std::accumulate(scaled_ex_intensities.cbegin(),
                            scaled_ex_intensities.cend(), _RealType{0})};
        auto discrete_parm = discrete_parm_t{scaled_ex_intensities};
        markov_parm_.emplace_back(std::make_pair(std::move(intensity_parm),
                                                 std::move(discrete_parm)));

        next_submodel(ex_intensities);
      }
      markov_parm_.shrink_to_fit();
    }

    template <typename _InputIterator>
    void __init(_InputIterator first, _InputIterator last) {
      static_assert(
          type_traits::is_input_iterator_v<_InputIterator>,
          "Class template rmolib::random::markovian_exmo_distribution<>: "
          "_InputIterator only be initialized with input iterator");
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
        "Class template rmolib::random::markovian_exmo_distribution<> must be "
        "parametrized with floating point type");
  };

  markovian_exmo_distribution() = default;

  template <typename _ForwardIterator>
  explicit markovian_exmo_distribution(const std::size_t dim,
                                       _ForwardIterator first,
                                       _ForwardIterator last)
      : parm_{dim, first, last} {}

  template <typename _Container>
  explicit markovian_exmo_distribution(const std::size_t dim,
                                       const _Container& ex_intensities)
      : parm_{dim, ex_intensities} {}

  explicit markovian_exmo_distribution(const param_type& parm) : parm_{parm} {}

  // Used for construction from a different specialization
  template <
      typename _ExMOParamType,
      std::enable_if_t<
          !std::is_convertible_v<_ExMOParamType, markovian_exmo_distribution> &&
              !std::is_convertible_v<_ExMOParamType, param_type> &&
              is_exmo_param_type_v<_ExMOParamType>,
          int> = 0>
  explicit markovian_exmo_distribution(_ExMOParamType&& parm)
      : parm_{std::forward<_ExMOParamType>(parm)} {}

  // compiler generated ctor and assignment op is sufficient

  void reset() {}

  auto min() const { return result_type(dim(), _RealType{0}); }
  auto max() const {
    return result_type(dim(), std::numeric_limits<_RealType>::infinity());
  }

  auto dim() const { return parm_.dim(); }
  auto ex_intensities() const { return parm_.ex_intensities(); }

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

  template <typename _Engine, typename _Container>
  void operator()(_Engine&& engine, const param_type& parm, _Container& out) {
    auto state = std::make_pair(_RealType{0}, std::size_t{0});
    auto& [time, number_dead] = state;
    auto dim = out.size();
    while (number_dead != dim) {
      std::size_t number_dead_before_transition = number_dead;
      state = __markov_process(engine, parm.markov_parm_, state);
      for (std::size_t i = number_dead_before_transition; i < number_dead; ++i)
        out[i] = time;
    }
    shuffle(out.begin(), out.end(), engine, uniform_int_dist_);
  }

  friend bool operator==(const markovian_exmo_distribution& lhs,
                         const markovian_exmo_distribution& rhs) {
    return lhs.parm_ == rhs.parm_;
  }

  friend bool operator!=(const markovian_exmo_distribution& lhs,
                         const markovian_exmo_distribution& rhs) {
    return !(lhs == rhs);
  }

 private:
  using markov_parm_t = typename param_type::markov_parm_t;

  param_type parm_{};
  _UniformIntDistribution uniform_int_dist_{};
  _ExponentialDistribution exponential_dist_{};
  _DiscreteDistribution discrete_dist_{};
  _ShuffleOperation shuffle{};

  template <typename _Engine>
  auto __markov_process(_Engine&& engine, const markov_parm_t& parm,
                        std::pair<_RealType, std::size_t> state) {
    auto& [time, location] = state;
    const auto& [intensity_parm, discrete_parm] = parm[location];
    time += exponential_dist_(engine, intensity_parm);
    location +=
        math::next_integral_value(discrete_dist_(engine, discrete_parm));
    return state;
  }

  static_assert(
      type_traits::is_safe_numeric_cast_v<
          _RealType, typename _ExponentialDistribution::result_type>,
      "Class template rmolib::random::markovian_exmo_distribution<> must be "
      "parametrized with exponential_distribution-type with matching "
      "result_type");

  static_assert(
      type_traits::is_safe_numeric_cast_v<
          std::size_t, typename _DiscreteDistribution::result_type>,
      "Class template rmolib::random::markovian_exmo_distribution<> must be "
      "parametrized with discrete_distribution-type with matching "
      "size_type");
};

/*
  // TODO: implement

  template <class _CharType, class _Traits, typename _RealType, typename
  _ExponentialDistribution> std::basic_ostream<_CharType, _Traits>&
  operator<<(std::basic_ostream<_CharType, _Traits>& os,
            markovian_exmo_distribution<_RealType,
  _ExponentialDistribution>& dist);

  template <class _CharType, class _Traits, typename _RealType, typename
  _ExponentialDistribution> std::basic_istream<_CharType, _Traits>&
  operator>>(std::basic_istream<_CharType, _Traits>& is,
             markovian_exmo_distribution<_RealType,
  _ExponentialDistribution>& dist);
*/

}  // namespace random

}  // namespace rmolib
