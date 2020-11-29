#pragma once

#include <cmath>
#include <limits>
#include <stdexcept>
#include <type_traits>
#include <utility>
#include <vector>

#include "rmolib/bit/bit_fill.hpp"
#include "rmolib/random/multivariate/internal/is_within.hpp"
#include "rmolib/random/multivariate/internal/mo_param_type.hpp"
#include "rmolib/random/univariate/exponential_distribution.hpp"
#include "rmolib/type_traits/is_safe_numeric_cast.hpp"
#include "rmolib/type_traits/iterator.hpp"

namespace rmolib {

namespace random {

template <typename _RealType, typename _ExponentialDistribution =
                                  exponential_distribution<_RealType>>
class esm_mo_distribution {
 public:
  using result_type = std::vector<_RealType>;

  class param_type {
   public:
    using distribution_type = esm_mo_distribution;

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
      const std::size_t size = bit::bit_fill<std::size_t>(0, dim_, true);
      std::vector<_RealType> out(size, _RealType{0});
      for (const auto& [i, shock_parm] : shocks_)
        out[i - 1] = shock_parm.lambda();
      return out;
    }

    friend class esm_mo_distribution;

    friend bool operator==(const param_type& lhs, const param_type& rhs) {
      return lhs.dim_ == rhs.dim_ && lhs.shocks_ == rhs.shocks_;
    }

    friend bool operator!=(const param_type& lhs, const param_type& rhs) {
      return !(lhs == rhs);
    }

   private:
    using exponential_parm_t = typename _ExponentialDistribution::param_type;

    std::size_t dim_{1};
    std::vector<std::pair<std::size_t,        // shock set (bit representation)
                          exponential_parm_t  // shock intensity
                          >>
        shocks_{};

    template <typename _ForwardIterator>
    void __validate_input(const std::size_t dim, _ForwardIterator first,
                          _ForwardIterator last) const {
      using std::distance;

      if (!(bit::bit_fill<std::size_t>(0, dim, true) == distance(first, last)))
        throw std::domain_error("intensities vector has wrong size");
    }

    void __init_empty() {
      shocks_ = {
          std::make_pair(std::size_t{1}, exponential_parm_t{_RealType{1}})};
    }

    template <typename _InputIterator>
    void __init_empty(_InputIterator first, _InputIterator last) {
      __validate_input(dim_, first, last);
      __init_empty();
    }

    template <typename _InputIterator>
    void __init(_InputIterator first, _InputIterator last,
                std::input_iterator_tag) {
      std::vector<_RealType> tmp(first, last);
      __init(tmp.cbegin(), tmp.cend());
    }

    template <typename _ForwardIterator>
    void __init(_ForwardIterator first, _ForwardIterator last,
                std::forward_iterator_tag) {
      using std::distance;

      __validate_input(dim_, first, last);

      shocks_.clear();
      shocks_.reserve(distance(first, last));
      for (auto it = first; it != last; ++it) {
        if (*it > 0)
          shocks_.emplace_back(
              std::make_pair(static_cast<std::size_t>(distance(first, it) + 1),
                             exponential_parm_t{*it}));
      }
      shocks_.shrink_to_fit();
    }

    template <typename _InputIterator>
    void __init(_InputIterator first, _InputIterator last) {
      static_assert(type_traits::is_input_iterator_v<_InputIterator>,
                    "Class template rmolib::random::esm_mo_distribution<>: "
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
        "Class template rmolib::random::esm_mo_distribution<> must be "
        "parametrized with floating point type");
  };

  esm_mo_distribution() = default;

  template <typename _InputIterator>
  explicit esm_mo_distribution(const std::size_t dim, _InputIterator first,
                               _InputIterator last)
      : parm_{dim, first, last} {}

  template <typename _Container>
  explicit esm_mo_distribution(const std::size_t dim,
                               const _Container& intensities)
      : parm_{dim, intensities} {}

  explicit esm_mo_distribution(const param_type& parm) : parm_{parm} {}

  // Used for construction from a different specialization
  template <typename _MOParamType,
            std::enable_if_t<
                !std::is_convertible_v<_MOParamType, esm_mo_distribution> &&
                    !std::is_convertible_v<_MOParamType, param_type> &&
                    is_mo_param_type_v<_MOParamType>,
                int> = 0>
  explicit esm_mo_distribution(_MOParamType&& parm)
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
    std::fill(out.begin(), out.end(),
              std::numeric_limits<_RealType>::infinity());
    const auto dim = out.size();
    for (const auto& [set, shock_parm] : parm.shocks_) {
      const auto time = exponential_dist_(engine, shock_parm);
      for (std::size_t i = 0; i < dim; ++i) {
        if (internal::is_within<std::size_t>(i, set))
          out[i] = std::min(out[i], time);
      }
    }
  }

  friend bool operator==(const esm_mo_distribution& lhs,
                         const esm_mo_distribution& rhs) {
    return lhs.parm_ == rhs.parm_;
  }

  friend bool operator!=(const esm_mo_distribution& lhs,
                         const esm_mo_distribution& rhs) {
    return !(lhs == rhs);
  }

 private:
  param_type parm_{};
  _ExponentialDistribution exponential_dist_{};

  static_assert(type_traits::is_safe_numeric_cast_v<
                    _RealType, typename _ExponentialDistribution::result_type>,
                "Class template rmolib::random::esm_mo_distribution<> must be "
                "parametrized with exponential_distribution-type with suitable "
                "result_type");
};

/*
  // TODO: implement

  template <class _CharType, class _Traits, typename _RealType, typename
  _ExponentialDistribution> std::basic_ostream<_CharType, _Traits>&
  operator<<(std::basic_ostream<_CharType, _Traits>& os,
            esm_mo_distribution<_RealType, _ExponentialDistribution>&
  dist);

  template <class _CharType, class _Traits, typename _RealType, typename
  _ExponentialDistribution> std::basic_istream<_CharType, _Traits>&
  operator>>(std::basic_istream<_CharType, _Traits>& is,
             esm_mo_distribution<_RealType, _ExponentialDistribution>&
  dist);
*/

}  // namespace random

}  // namespace rmolib
