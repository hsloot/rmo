#pragma once

#include <algorithm>
#include <limits>
#include <type_traits>

#include "rmolib/random/univariate/exponential_distribution.hpp"

namespace rmolib {

namespace random {

// type_trait for identifying possible alternative implementations of
// a cuadras_auge_distribution<>::param_type
template <typename _T, class = void>
struct is_cuadras_auge_param_type : public std::false_type {};

template <typename _T>
struct is_cuadras_auge_param_type<
    _T,
    typename std::enable_if<
        decltype(std::declval<_T&>().dim(), std::true_type())::value&& decltype(
            std::declval<_T&>().alpha(),
            std::true_type())::value&& decltype(std::declval<_T&>().beta(),
                                                std::true_type())::value>::type>
    : public std::true_type {};

template <typename _T>
constexpr bool is_cuadras_auge_param_type_v =
    is_cuadras_auge_param_type<_T>::value;

template <typename _Container,
          typename _ExponentialDistribution =
              exponential_distribution<typename _Container::value_type>>
class cuadras_auge_distribution {
 public:
  using result_type = _Container;
  using value_type = typename _Container::value_type;
  using size_type = typename _Container::size_type;

  class param_type {
   public:
    using distribution_type = cuadras_auge_distribution;

    param_type() = default;

    explicit param_type(size_type dim, const value_type alpha,
                        const value_type beta)
        : dim_{dim}, alpha_parm_{alpha}, beta_parm_{beta} {}

    // Used for construction from a different specialization
    template <typename _CuadrasAugeParamType,
              typename std::enable_if<
                  !std::is_convertible_v<_CuadrasAugeParamType, param_type> &&
                      is_cuadras_auge_param_type_v<_CuadrasAugeParamType>,
                  int>::type = 0>
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
    using exponential_param_type =
        typename _ExponentialDistribution::param_type;
    size_type dim_{1};
    exponential_param_type alpha_parm_{value_type{1}};
    exponential_param_type beta_parm_{value_type{0}};
  };

  cuadras_auge_distribution() = default;

  explicit cuadras_auge_distribution(size_type dim, const value_type alpha,
                                     const value_type beta)
      : parm_{dim, alpha, beta} {}

  explicit cuadras_auge_distribution(const param_type& parm) : parm_{parm} {}

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
  auto alpha() const { return parm_.alpha(); }
  auto beta() const { return parm_.beta(); }

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

  template <typename _Engine, typename _OutputContainer>
  void operator()(_Engine& engine, const param_type& parm,
                  _OutputContainer& out) {
    // reference must be remove to use Rcpp::NumericVector or
    // Rcpp::MatrixRow<REALSXP> as _OutputContainer
    using value_type =
        std::remove_reference_t<typename _OutputContainer::value_type>;

    std::fill(out.begin(), out.end(),
              std::numeric_limits<value_type>::infinity());
    auto global_shock = exponential_distribution_(engine, parm.beta_parm_);
    for (auto& value : out) {
      auto individual_shock =
          exponential_distribution_(engine, parm.alpha_parm_);
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
  _ExponentialDistribution exponential_distribution_{};

  static_assert(
      std::is_same<value_type,
                   typename _ExponentialDistribution::result_type>::value,
      "Class template rmolib::random::cuadras_auge_distribution<> must be "
      "parametrized with unit_exponential_distribution-type with matching "
      "result_type");
};

/*
  // TODO: implement

  template <class _CharType, class _Traits, typename _Container, typename
  _ExponentialDistribution> std::basic_ostream<_CharType, _Traits>&
  operator<<(std::basic_ostream<_CharType, _Traits>& os,
            cuadras_auge_distribution<_Container, _ExponentialDistribution>&
  dist);

  template <class _CharType, class _Traits, typename _Container, typename
  _ExponentialDistribution> std::basic_istream<_CharType, _Traits>&
  operator>>(std::basic_istream<_CharType, _Traits>& is,
             cuadras_auge_distribution<_Container,
  _ExponentialDistribution>& dist);
*/

}  // namespace random

}  // namespace rmolib
