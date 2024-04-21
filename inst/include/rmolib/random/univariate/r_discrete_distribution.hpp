#pragma once

#include <cmath>
#include <limits>
#include <numeric>
#include <stdexcept>
#include <type_traits>
#include <vector>

#include "rmolib/algorithm/r_sort.hpp"
#include "rmolib/random/univariate/internal/discrete_param_type.hpp"
#include "rmolib/type_traits/is_safe_numeric_cast.hpp"

namespace rmolib {

namespace random {

template <typename _IntType, typename _WeightType,
          typename _UnitUniformRealDistribution>
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
            : param_type{p.cbegin(), p.cend()} {}

        param_type(std::initializer_list<_WeightType> wl)
            : param_type{wl.begin(), wl.end()} {}

        template <class _UnaryFunctor>
        explicit param_type(typename std::vector<_WeightType>::size_type count,
                            const _WeightType xmin,  // cSpell:ignore xmin
                            const _WeightType xmax,  // cSpell:ignore xmax
                            _UnaryFunctor unary_op) {
            using size_t = typename std::vector<_WeightType>::size_type;

            count += static_cast<size_t>(count == 0);
            const double delta = (xmax - xmin) / count;

            std::vector<_WeightType> tmp;
            tmp.reserve(count);
            for (auto k = std::size_t{0}; k < count; ++k) {
                tmp.emplace_back(unary_op(xmin + k * delta + delta / 2));
            }
            __init(tmp.cbegin(), tmp.cend());
        }

        // Used for construction from a different specialization
        template <typename _DiscreteParamType,
                  std::enable_if_t<
                      !std::is_convertible_v<_DiscreteParamType, param_type> &&
                          is_discrete_param_type_v<_DiscreteParamType>,
                      int> = 0>
        explicit param_type(_DiscreteParamType&& parm)
            : param_type{parm.probabilities()} {}

        // compiler generated ctor and assignment op is sufficient

        auto probabilities() const {
            std::vector<std::pair<_IntType, _WeightType>> probabilities{};
            probabilities.reserve(cumulative_probabilities_.size());
            std::adjacent_difference(
                cumulative_probabilities_.cbegin(),
                cumulative_probabilities_.cend(),
                std::back_inserter(probabilities),
                [](const auto& val, const auto& acc) {
                    return std::make_pair(std::get<0>(val),
                                          std::get<1>(val) - std::get<1>(acc));
                });

            std::sort(probabilities.begin(), probabilities.end(),
                      [](const auto& l, const auto& u) {
                          return std::get<0>(l) < std::get<0>(u);
                      });

            std::vector<_WeightType> out;
            out.reserve(probabilities.size());
            for (const auto& [_ignore, p] : probabilities) out.emplace_back(p);
            out.shrink_to_fit();

            return out;
        }

        friend class r_discrete_distribution;

        friend bool operator==(const param_type& lhs, const param_type& rhs) {
            return lhs.cumulative_probabilities_ ==
                   rhs.cumulative_probabilities_;
        }

        friend bool operator!=(const param_type& lhs, const param_type& rhs) {
            return !(lhs == rhs);
        }

       private:
        std::vector<std::pair<_IntType, _WeightType>>
            cumulative_probabilities_{};

        template <class _ForwardIterator>
        void __validate_input(_ForwardIterator first,
                              _ForwardIterator last) const {
            if (!std::all_of(first, last, [](const auto v) { return v >= 0; }))
                throw std::domain_error("negative probabilities not allowed");
            if (std::all_of(first, last, [](const auto v) { return v == 0; }))
                throw std::domain_error("all probabilities are zero");
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
            using std::distance;

            __validate_input(first, last);
            cumulative_probabilities_.clear();
            cumulative_probabilities_.reserve(distance(first, last));

            for (auto [it, idx] = std::make_pair(first, _IntType{0});
                 it != last; ++it, ++idx)
                cumulative_probabilities_.emplace_back(
                    std::make_pair(idx, *it));
            cumulative_probabilities_.shrink_to_fit();

            // sort probabilities in descending order
            algorithm::r_sort(cumulative_probabilities_.begin(),
                              cumulative_probabilities_.end(),
                              [](const auto& l, const auto& u) {
                                  return std::get<1>(l) > std::get<1>(u);
                              });

            // accumulate probabilities
            std::partial_sum(cumulative_probabilities_.cbegin(),
                             cumulative_probabilities_.cend(),
                             cumulative_probabilities_.begin(),
                             [](const auto& sum, const auto& val) {
                                 return std::make_pair(
                                     std::get<0>(val),
                                     std::get<1>(sum) + std::get<1>(val));
                             });

            // normalize probabilities
            std::transform(
                cumulative_probabilities_.cbegin(),
                cumulative_probabilities_.cend(),
                cumulative_probabilities_.begin(),
                [mass = std::get<1>(cumulative_probabilities_.back())](
                    const auto& val) {
                    return std::make_pair(std::get<0>(val),
                                          std::get<1>(val) / mass);
                });
        }

        template <typename _InputIterator>
        void __init(_InputIterator first, _InputIterator last) {
            if (first == last) {
                __init_empty();
            } else {
                using category = typename std::iterator_traits<
                    _InputIterator>::iterator_category;
                __init(first, last, category{});
            }
        }

        static_assert(
            std::is_integral_v<_IntType>,
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

    r_discrete_distribution(std::initializer_list<_WeightType> wl) : parm_{wl} {
        init_unit_uniform_real_distribution();
    }

    // template <class UnaryOperation>
    template <typename _UnaryOperation>
    explicit r_discrete_distribution(
        typename std::vector<_WeightType>::size_type count,
        const _WeightType xmin, const _WeightType xmax,
        _UnaryOperation unary_op)
        : parm_{count, xmin, xmax, unary_op} {
        init_unit_uniform_real_distribution();
    }

    explicit r_discrete_distribution(const param_type& parm) : parm_{parm} {
        init_unit_uniform_real_distribution();
    }

    // Used for construction from a different specialization
    template <typename _DiscreteParamType,
              std::enable_if_t<
                  !std::is_convertible_v<_DiscreteParamType,
                                         r_discrete_distribution> &&
                      !std::is_convertible_v<_DiscreteParamType, param_type> &&
                      is_discrete_param_type_v<_DiscreteParamType>,
                  int> = 0>
    explicit r_discrete_distribution(_DiscreteParamType&& parm)
        : parm_{std::forward<_DiscreteParamType>(parm)} {
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

    template <typename _Engine>
    result_type operator()(_Engine&& engine) {
        return (*this)(std::forward<_Engine>(engine), parm_);
    }

    template <typename _Engine>
    result_type operator()(_Engine&& engine, const param_type& parm) {
        const auto u =
            unit_uniform_real_distribution_(std::forward<_Engine>(engine));
        const auto it =
            std::lower_bound(parm.cumulative_probabilities_.cbegin(),
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
        return !(lhs == rhs);
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
            using unit_param_type =
                typename _UnitUniformRealDistribution::param_type;
            unit_uniform_real_distribution_.param(unit_param_type{0., 1.});
        }
    }

    static_assert(
        type_traits::is_safe_numeric_cast_v<
            _WeightType, typename _UnitUniformRealDistribution::result_type>,
        "Class template rmolib::random::r_discrete_distribution<> must be "
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
