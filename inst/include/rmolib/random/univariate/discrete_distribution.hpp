#pragma once

#include <algorithm>
#include <cmath>
#include <numeric>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <vector>

#include "rmolib/random/univariate/internal/discrete_param_type.hpp"
#include "rmolib/type_traits/is_safe_numeric_cast.hpp"

namespace rmolib {

namespace random {

template <typename _IntType, typename _WeightType,
          typename _UnitUniformRealDistribution,
          typename _UniformIntDistribution>
class discrete_distribution {
   public:
    using result_type = _IntType;

    class param_type {
       public:
        using distribution_type = discrete_distribution;

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
            auto probs = std::vector<_WeightType>(n_, _WeightType{0});
            for (auto [i, it] =
                     std::make_pair(std::size_t{0}, alias_table_.cbegin());
                 it != alias_table_.cend(); ++it, ++i) {
                const auto& [j, w] = *it;
                probs[i] += w;
                probs[j] += _WeightType{1} - w;
            }
            std::transform(probs.cbegin(), probs.cend(), probs.begin(),
                           [mass = accumulate(probs.cbegin(), probs.cend(),
                                              _WeightType{0})](const auto val) {
                               return val / mass;
                           });

            return probs;
        }

        friend class discrete_distribution;

        friend bool operator==(const param_type& lhs, const param_type& rhs) {
            return lhs.n_ == rhs.n_ && lhs.alias_table_ == rhs.alias_table_;
        }

        friend bool operator!=(const param_type& lhs, const param_type& rhs) {
            return !(lhs == rhs);
        }

       private:
        std::size_t n_{};
        std::vector<std::pair<_IntType, _WeightType>> alias_table_{};

        template <typename _ForwardIterator>
        void __validate_input(_ForwardIterator first,
                              _ForwardIterator last) const {
            if (!std::all_of(first, last, [](const auto v) { return v >= 0; }))
                throw std::domain_error("negative probabilities not allowed");
            if (std::all_of(first, last, [](const auto v) { return v == 0; }))
                throw std::domain_error("all probabilities are zero");
        }

        void __init_empty() {
            n_ = std::size_t{1};
            alias_table_.clear();
            alias_table_.emplace_back(_IntType{0}, _WeightType{1});
            alias_table_.shrink_to_fit();
        }

        template <typename _InputIterator>
        void __init(_InputIterator first, _InputIterator last,
                    std::input_iterator_tag) {
            auto tmp = std::vector<_WeightType>(first, last);
            __init(tmp.cbegin(), tmp.cend());
        }

        template <typename _ForwardIterator>
        void __init(_ForwardIterator first, _ForwardIterator last,
                    std::forward_iterator_tag) {
            using std::distance;

            __validate_input(first, last);
            n_ = distance(first, last);
            auto tmp = std::vector<_WeightType>{};
            tmp.reserve(n_);
            std::transform(
                first, last, std::back_inserter(tmp),
                [n = n_, mass = std::accumulate(first, last, _WeightType{0})](
                    const auto val) {
                    return val / mass * static_cast<_WeightType>(n);
                });

            auto lower = std::vector<std::pair<_IntType, _WeightType>>{};
            lower.reserve(n_);
            auto upper = std::vector<std::pair<_IntType, _WeightType>>{};
            upper.reserve(n_);

            alias_table_.clear();
            alias_table_.resize(n_);
            for (auto&& [i, it] = std::make_pair(_IntType{0}, tmp.cbegin());
                 it != tmp.cend(); ++i, ++it) {
                const auto w = *it;
                if (w < _WeightType{1}) {
                    lower.emplace_back(std::make_pair(i, w));
                } else if (w > _WeightType{1}) {
                    upper.emplace_back(std::make_pair(i, w));
                } else {
                    alias_table_[i] = std::make_pair(i, _WeightType{1});
                }
            }

            while (!upper.empty() && !lower.empty()) {
                const auto [il, wl] = lower.back();
                lower.pop_back();
                auto [iu, wu] = upper.back();
                upper.pop_back();
                alias_table_[il] = std::make_pair(iu, wl);
                wu += (wl - _WeightType{1});
                if (wu < _WeightType{1}) {
                    lower.emplace_back(std::make_pair(iu, wu));
                } else if (wu > _WeightType{1}) {
                    upper.emplace_back(std::make_pair(iu, wu));
                } else {
                    alias_table_[iu] = std::make_pair(iu, _WeightType{1});
                }
            }
            for (const auto& [i, w] : lower)
                alias_table_[i] = std::make_pair(i, _WeightType{1});
            for (const auto& [i, w] : upper)
                alias_table_[i] = std::make_pair(i, _WeightType{1});
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
            "Class template rmolib::random::discrete_distribution<> must be "
            "parametrized with integral type");
    };

    discrete_distribution() { init_unit_uniform_real_distribution(); }

    template <typename _InputIterator>
    explicit discrete_distribution(_InputIterator first, _InputIterator last)
        : parm_{first, last} {
        init_unit_uniform_real_distribution();
    }

    explicit discrete_distribution(const std::vector<_WeightType>& p)
        : parm_{p} {
        init_unit_uniform_real_distribution();
    }

    discrete_distribution(std::initializer_list<_WeightType> wl) : parm_{wl} {
        init_unit_uniform_real_distribution();
    }

    // template <class UnaryOperation>
    template <typename _UnaryOperation>
    explicit discrete_distribution(
        typename std::vector<_WeightType>::size_type count,
        const _WeightType xmin, const _WeightType xmax,
        _UnaryOperation unary_op)
        : parm_{count, xmin, xmax, unary_op} {
        init_unit_uniform_real_distribution();
    }

    explicit discrete_distribution(const param_type& parm) : parm_{parm} {
        init_unit_uniform_real_distribution();
    }

    // Used for construction from a different specialization
    template <
        typename _DiscreteParamType,
        std::enable_if_t<
            !std::is_convertible_v<_DiscreteParamType, discrete_distribution> &&
                !std::is_convertible_v<_DiscreteParamType, param_type> &&
                is_discrete_param_type_v<_DiscreteParamType>,
            int> = 0>
    explicit discrete_distribution(_DiscreteParamType&& parm)
        : parm_{std::forward<_DiscreteParamType>(parm)} {
        init_unit_uniform_real_distribution();
    }

    // compiler generated ctor and assignment op is sufficient

    void reset() {}

    auto min() const { return result_type{0}; }
    auto max() const { return parm_.n_ - result_type{1}; }

    auto probabilities() const { return parm_.probabilities(); }

    param_type param() const { return parm_; }
    void param(const param_type& parm) { parm_ = parm; }

    template <typename _Engine>
    result_type operator()(_Engine&& engine) {
        return (*this)(std::forward<_Engine>(engine), parm_);
    }

    template <typename _Engine>
    result_type operator()(_Engine&& engine, const param_type& parm) {
        using uniform_int_parm_t = typename _UniformIntDistribution::param_type;

        const auto uniform_int_parm =
            uniform_int_parm_t{0, parm.alias_table_.size()};
        const auto i = uniform_int_dist_(engine, uniform_int_parm);
        const auto& [j, p] = parm.alias_table_[i];
        auto out = i;
        if (p < _WeightType{1}) {
            const auto u = unit_uniform_real_dist_(engine);
            if (u > p) out = j;
        }

        return out;
    }

    friend bool operator==(const discrete_distribution& lhs,
                           const discrete_distribution& rhs) {
        return lhs.parm_ == rhs.parm_;
    }

    friend bool operator!=(const discrete_distribution& lhs,
                           const discrete_distribution& rhs) {
        return !(lhs == rhs);
    }

   private:
    param_type parm_{};
    _UnitUniformRealDistribution unit_uniform_real_dist_{};
    _UniformIntDistribution uniform_int_dist_{};

    void init_unit_uniform_real_distribution() {
        if constexpr (std::is_constructible_v<_UnitUniformRealDistribution,
                                              const _WeightType,
                                              const _WeightType>) {
            // TODO:
            // static_assert(is_distribution_type<_UnitUniformRealDistribution>)
            using unit_param_type =
                typename _UnitUniformRealDistribution::param_type;
            unit_uniform_real_dist_.param(unit_param_type{0., 1.});
        }
    }

    static_assert(
        type_traits::is_safe_numeric_cast_v<
            _WeightType, typename _UnitUniformRealDistribution::result_type>,
        "Class template rmolib::random::discrete_distribution<> must be "
        "parametrized with unit_uniform_real_distribution-type with matching "
        "result_type and _WeightType");

    static_assert(
        type_traits::is_safe_numeric_cast_v<
            std::size_t, typename _UniformIntDistribution::result_type>,
        "Class template rmolib::random::discrete_distribution<> must be "
        "parametrized with uniform_int_distribution-type with matching "
        "result_type and std::size_t");
};

/*
  // TODO: implement

  template <class _CharType, class _Traits, typename _IntType, typename
  _WeightType, typename _UnitUniformRealDistribution, _UniformIntDistribution>
  std::basic_ostream<_CharType, _Traits>&
  operator<<(std::basic_ostream<_CharType, _Traits>& os,
            discrete_distribution<_IntType, _WeightType,
  _UnitUniformRealDistribution>& dist);

  template <class _CharType, class _Traits, typename _IntType, typename
  _WeightType, typename _UnitUniformRealDistribution, _UniformIntDistribution>
  std::basic_istream<_CharType, _Traits>&
  operator>>(std::basic_istream<_CharType, _Traits>& is,
             discrete_distribution<_IntType, _WeightType,
  _UnitUniformRealDistribution>& dist);
*/

}  // namespace random

}  // namespace rmolib
