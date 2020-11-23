#pragma once

#include <cmath>
#include <limits>
#include <stdexcept>
#include <type_traits>
#include <vector>

#include "rmolib/algorithm/r_sort.hpp"
#include "rmolib/random/univariate/uniform_real_distribution.hpp"

namespace rmolib {

namespace random {

// type_trait for identifying possible alternative implementations of
// a uniform_real_distribution<>::param_type
template <typename _T, class = void>
struct is_discrete_param_type : public std::false_type {};

template <typename _T>
struct is_discrete_param_type<
    _T, typename std::enable_if<decltype(std::declval<_T&>().probabilities(),
                                         std::true_type())::value>::type>
    : public std::true_type {};

template <typename _T>
constexpr bool is_discrete_param_type_v = is_discrete_param_type<_T>::value;

template <typename _IntType, typename _WeightType,
          typename _UnitUniformRealDistribution =
              uniform_real_distribution<_WeightType>>
class r_discrete_distribution {
 public:
  using result_type = _IntType;

  class param_type {
   public:
    using distribution_type = r_discrete_distribution;

    param_type() { __init_empty(); }

    template <typename _InputIterator>
    explicit param_type(_InputIterator first, _InputIterator last) {
      __init(first, last);
    }

    explicit param_type(const std::vector<_WeightType>& p)
        : param_type{p.begin(), p.end()} {}

    param_type(std::initializer_list<_WeightType> wl)
        : param_type{wl.begin(), wl.end()} {}

    template <class _UnaryFunctor>
    param_type(typename std::vector<_WeightType>::size_type count,
               _WeightType xmin, _WeightType xmax, _UnaryFunctor unary_op) {
      using size_t = typename std::vector<_WeightType>::size_type;

      count += static_cast<size_t>(count == 0);
      double delta = (xmax - xmin) / count;

      std::vector<_WeightType> tmp;
      tmp.reserve(count);
      for (size_t k = 0; k < count; ++k) {
        tmp.emplace_back(unary_op(xmin + k * delta + delta / 2));
      }
      __init(tmp.cbegin(), tmp.cend());
    }

    // Used for construction from a different specialization
    template <typename _DiscreteParamType,
              typename std::enable_if<
                  !std::is_convertible_v<_DiscreteParamType, param_type> &&
                      is_discrete_param_type_v<_DiscreteParamType>,
                  int>::type = 0>
    explicit param_type(_DiscreteParamType&& parm)
        : param_type{parm.probabilities()} {}

    // compiler generated ctor and assignment op is sufficient

    auto probabilities() const {
      std::vector<std::pair<_IntType, _WeightType>> probabilities{};
      probabilities.reserve(cumulative_probabilities_.size());

      std::adjacent_difference(
          cumulative_probabilities_.cbegin(), cumulative_probabilities_.cend(),
          std::back_inserter(probabilities), [](const auto& x, const auto& y) {
            return std::make_pair(std::get<0>(x),
                                  std::get<1>(x) - std::get<1>(y));
          });

      std::sort(probabilities.begin(), probabilities.end(),
                [](const auto& l, const auto& u) {
                  return std::get<0>(l) < std::get<0>(u);
                });

      std::vector<_WeightType> out;
      out.reserve(probabilities.size());
      for (const auto& p : probabilities)
        out.emplace_back(std::move(std::get<1>(p)));

      return out;
    }

    friend class r_discrete_distribution;

    friend bool operator==(const param_type& lhs, const param_type& rhs) {
      return lhs.cumulative_probabilities_ == rhs.cumulative_probabilities_;
    }

    friend bool operator!=(const param_type& lhs, const param_type& rhs) {
      return !(lhs == rhs);
    }

   private:
    std::vector<std::pair<_IntType, _WeightType>> cumulative_probabilities_{};

    template <class _ForwardIterator>
    void __validate_input(_ForwardIterator first, _ForwardIterator last) const {
      if (!std::all_of(first, last, [](const auto v) { return v >= 0; }))
        throw std::domain_error("negative probabilities not allowed");
    }

    void __init_empty() {
      cumulative_probabilities_.clear();
      cumulative_probabilities_.emplace_back(
          std::make_pair(_IntType{0}, _WeightType{1}));
      cumulative_probabilities_.shrink_to_fit();
    }

    template <class _InputIterator>
    void __init(_InputIterator first, _InputIterator last,
                std::input_iterator_tag) {
      std::vector<_WeightType> tmp(first, last);
      __init(tmp.begin(), tmp.end());
    }

    template <class _ForwardIterator>
    void __init(_ForwardIterator first, _ForwardIterator last,
                std::forward_iterator_tag) {
      __validate_input(first, last);
      cumulative_probabilities_.reserve(std::distance(first, last));

      _IntType idx{0};
      for (auto it = first; it != last; ++it) {
        if (*it < 0) throw std::domain_error("negative probability");
        cumulative_probabilities_.emplace_back(std::make_pair(idx++, *it));
      }

      // sort probabilities in descending order
      algorithm::r_sort(cumulative_probabilities_.begin(),
                        cumulative_probabilities_.end(),
                        [](const auto& l, const auto& u) {
                          return std::get<1>(l) > std::get<1>(u);
                        });

      // accumulate probabilities
      std::partial_sum(
          cumulative_probabilities_.cbegin(), cumulative_probabilities_.cend(),
          cumulative_probabilities_.begin(), [](const auto& x, const auto& y) {
            return std::make_pair(std::get<0>(y),
                                  std::get<1>(x) + std::get<1>(y));
          });

      // normalize probabilities
      std::transform(
          cumulative_probabilities_.cbegin(), cumulative_probabilities_.cend(),
          cumulative_probabilities_.begin(),
          [total =
               std::get<1>(cumulative_probabilities_.back())](const auto& cp) {
            return std::make_pair(std::get<0>(cp), std::get<1>(cp) / total);
          });
    }

    template <typename _InputIterator>
    void __init(_InputIterator first, _InputIterator last) {
      if (first == last) {
        __init_empty();
      } else {
        using category =
            typename std::iterator_traits<_InputIterator>::iterator_category;
        __init(first, last, category{});
      }
    }

    static_assert(
        std::is_integral<_IntType>::value,
        "Class template rmolib::random::r_discrete_distribution<> must be "
        "parametrized with integral type");
  };

  r_discrete_distribution() = default;

  template <typename _InputIterator>
  explicit r_discrete_distribution(_InputIterator first, _InputIterator last)
      : parm_{first, last} {
    init_unit_uniform_real_distribution();
  }

  explicit r_discrete_distribution(const std::vector<_WeightType>& p)
      : parm_{p} {
    init_unit_uniform_real_distribution();
  }

  r_discrete_distribution(std::initializer_list<_WeightType>& wl)
      : parm_{wl.begin(), wl.end()} {
    init_unit_uniform_real_distribution();
  }

  // template <class UnaryOperation>
  template <typename _UnaryOperation>
  r_discrete_distribution(typename std::vector<_WeightType>::size_type count,
                          _WeightType xmin, _WeightType xmax,
                          _UnaryOperation unary_op)
      : parm_{count, xmin, xmax, unary_op} {
    init_unit_uniform_real_distribution();
  }

  explicit r_discrete_distribution(const param_type& parm) : parm_{parm} {
    init_unit_uniform_real_distribution();
  }

  // compiler generated ctor and assignment op is sufficient

  void reset() {}

  auto min() const { return result_type{0}; }
  auto max() const {
    return parm_.cumulative_probabilities_.size() - result_type{1};
  }

  auto probabilities() const { return parm_.probabilities(); }

  param_type param() const { return parm_; }
  void param(const param_type& parm) { parm_ = parm; }

  template <typename _EngineType>
  result_type operator()(_EngineType& engine) {
    return (*this)(engine, parm_);
  }

  template <typename _EngineType>
  result_type operator()(_EngineType& engine, const param_type& parm) {
    if (parm.cumulative_probabilities_.empty()) return 0;
    auto u = unit_uniform_real_distribution_(engine);
    auto it = std::lower_bound(parm.cumulative_probabilities_.cbegin(),
                               parm.cumulative_probabilities_.cend(), u,
                               [](const auto& element, const auto& value) {
                                 return std::get<1>(element) < value;
                               });

    return std::get<0>(*it);
  }

  friend bool operator==(const r_discrete_distribution& lhs,
                         const r_discrete_distribution& rhs) {
    return lhs.parm_ == rhs.parm_;
  }

  friend bool operator!=(const r_discrete_distribution& lhs,
                         const r_discrete_distribution& rhs) {
    return lhs.parm_ != rhs.parm_;
  }

 private:
  param_type parm_{};
  _UnitUniformRealDistribution unit_uniform_real_distribution_{};

  void init_unit_uniform_real_distribution() {
    if constexpr (std::is_constructible_v<_UnitUniformRealDistribution,
                                          const _WeightType,
                                          const _WeightType>) {
      // TODO:
      // static_assert(is_distribution_type<_UnitUniformRealDistribution>)
      using unit_param_type = typename _UnitUniformRealDistribution::param_type;
      unit_uniform_real_distribution_.param(unit_param_type{0., 1.});
    }
  }

  static_assert(
      std::is_same<_WeightType,
                   typename _UnitUniformRealDistribution::result_type>::value,
      "Class template rmolib::random::discrete_distribution<> must be "
      "parametrized with unit_uniform_real_distribution-type with matching "
      "result_type and _WeightType");
};

/*
  // TODO: implement

  template <class _CharType, class _Traits, typename _IntType, typename
  _WeighType, typename _UnitUniformRealDistribution>
  std::basic_ostream<_CharType, _Traits>&
  operator<<(std::basic_ostream<_CharType, _Traits>& os,
            r_discrete_distribution<_IntType, _WeightType,
  _UnitUniformRealDistribution>& dist);

  template <class _CharType, class _Traits, typename _IntType, typename
  _WeighType, typename _UnitUniformRealDistribution>
  std::basic_istream<_CharType, _Traits>&
  operator>>(std::basic_istream<_CharType, _Traits>& is,
             r_discrete_distribution<_IntType, _WeightType,
  _UnitUniformRealDistribution>& dist);
*/

}  // namespace random

}  // namespace rmolib
