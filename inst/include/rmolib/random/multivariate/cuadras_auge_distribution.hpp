#pragma once

#include <algorithm>
#include <limits>
#include <type_traits>

#include "rmolib/type_traits/is_safe_numeric_cast.hpp"

namespace rmolib {

namespace random {

namespace internal {

template <typename _T, class = void>
struct __is_cuadras_auge_param_type : public std::false_type {};

template <typename _T>
struct __is_cuadras_auge_param_type<
    _T,
    std::enable_if_t<
        decltype(std::declval<_T>().dim(), std::true_type())::value&& decltype(
            std::declval<_T>().alpha(),
            std::true_type())::value&& decltype(std::declval<_T>().beta(),
                                                std::true_type())::value>>
    : public std::true_type {};

}  // namespace internal

template <typename _T>
struct is_cuadras_auge_param_type
    : public internal::__is_cuadras_auge_param_type<std::remove_cv_t<_T>> {};

//! true, if _T can be used to construct cuadras_auge_distribution<>::param_type
template <typename _T>
constexpr bool is_cuadras_auge_param_type_v =
    is_cuadras_auge_param_type<_T>::value;

template <typename _RealType, typename _ExponentialDistribution>
class cuadras_auge_distribution {
 public:
  using result_type = std::vector<_RealType>;

  class param_type {
   public:
    using distribution_type = cuadras_auge_distribution;

    param_type() = default;

    explicit param_type(const std::size_t dim, const _RealType alpha,
                        const _RealType beta)
        : dim_{dim}, alpha_parm_{alpha}, beta_parm_{beta} {}

    // Used for construction from a different specialization
    template <typename _CuadrasAugeParamType,
              std::enable_if_t<
                  !std::is_convertible_v<_CuadrasAugeParamType, param_type> &&
                      is_cuadras_auge_param_type_v<_CuadrasAugeParamType>,
                  int> = 0>
    explicit param_type(_CuadrasAugeParamType&& parm)
        : param_type{parm.dim(), parm.alpha(), parm.beta()} {}

    // compiler generated ctor and assignment op is sufficient

    auto dim() const { return dim_; }
    auto alpha() const { return alpha_parm_.lambda(); }
    auto beta() const { return beta_parm_.lambda(); }

    friend class cuadras_auge_distribution;

    friend bool operator==(const param_type& lhs, const param_type& rhs) {
      return lhs.dim_ == rhs.dim_ && lhs.alpha_parm_ == rhs.alpha_parm_ &&
             lhs.beta_parm_ == rhs.beta_parm_;
    }

    friend bool operator!=(const param_type& lhs, const param_type& rhs) {
      return !(lhs == rhs);
    }

   private:
    using exponential_parm_t = typename _ExponentialDistribution::param_type;

    std::size_t dim_{1};
    exponential_parm_t alpha_parm_{_RealType{1}};
    exponential_parm_t beta_parm_{_RealType{0}};
  };

  cuadras_auge_distribution() = default;

  explicit cuadras_auge_distribution(const std::size_t dim,
                                     const _RealType alpha,
                                     const _RealType beta)
      : parm_{dim, alpha, beta} {}

  explicit cuadras_auge_distribution(const param_type& parm) : parm_{parm} {}

  // Used for construction from a different specialization
  template <typename _CuadrasAugeParamType,
            std::enable_if_t<
                !std::is_convertible_v<_CuadrasAugeParamType,
                                       cuadras_auge_distribution> &&
                    !std::is_convertible_v<_CuadrasAugeParamType, param_type> &&
                    is_cuadras_auge_param_type_v<_CuadrasAugeParamType>,
                int> = 0>
  explicit cuadras_auge_distribution(_CuadrasAugeParamType&& parm)
      : parm_{std::forward<_CuadrasAugeParamType>(parm)} {}

  // compiler generated ctor and assignment op is sufficient

  void reset() {}

  auto min() const { return result_type(dim(), _RealType{0}); }
  auto max() const {
    return result_type(dim(), std::numeric_limits<_RealType>::infinity());
  }

  auto dim() const { return parm_.dim(); }
  auto alpha() const { return parm_.alpha(); }
  auto beta() const { return parm_.beta(); }

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

  template <typename _Engine, typename _OutputContainer>
  void operator()(_Engine&& engine, const param_type& parm,
                  _OutputContainer& out) {
    const auto global_shock = exponential_dist_(engine, parm.beta_parm_);
    for (auto& value : out) {
      const auto individual_shock = exponential_dist_(engine, parm.alpha_parm_);
      value = std::min(individual_shock, global_shock);
    }
  }

  friend bool operator==(const cuadras_auge_distribution& lhs,
                         const cuadras_auge_distribution& rhs) {
    return lhs.parm_ == rhs.parm_;
  }

  friend bool operator!=(const cuadras_auge_distribution& lhs,
                         const cuadras_auge_distribution& rhs) {
    return !(lhs == rhs);
  }

 private:
  param_type parm_{};
  _ExponentialDistribution exponential_dist_{};

  static_assert(
      type_traits::is_safe_numeric_cast_v<
          _RealType, typename _ExponentialDistribution::result_type>,
      "Class template rmolib::random::cuadras_auge_distribution<> must be "
      "parametrized with unit_exponential_distribution-type with suitable "
      "result_type");
};

/*
  // TODO: implement

  template <class _CharType, class _Traits, typename _RealType, typename
  _ExponentialDistribution> std::basic_ostream<_CharType, _Traits>&
  operator<<(std::basic_ostream<_CharType, _Traits>& os,
            cuadras_auge_distribution<_RealType, _ExponentialDistribution>&
  dist);

  template <class _CharType, class _Traits, typename _RealType, typename
  _ExponentialDistribution> std::basic_istream<_CharType, _Traits>&
  operator>>(std::basic_istream<_CharType, _Traits>& is,
             cuadras_auge_distribution<_RealType,
  _ExponentialDistribution>& dist);
*/

}  // namespace random

}  // namespace rmolib
