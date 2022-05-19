#pragma once

#include <cstddef>
#include <iterator>
#include <type_traits>
#include <utility>
#include <vector>

#include <Rcpp.h>

static const R_xlen_t C_CHECK_USR_INTERRUP = 100000;

template <typename T, class = void>
struct has_value_type : public std::false_type {};

template <typename T>
struct has_value_type<T, std::void_t<typename T::value_type>>
    : public std::true_type {};

template <typename T>
constexpr bool has_value_type_v = has_value_type<T>::value;

template <typename T, class = void>
struct has_allocator_type : public std::false_type {};

template <typename T>
struct has_allocator_type<T, std::void_t<typename T::allocator_type>>
    : public std::true_type {};

template <typename T>
constexpr bool has_allocator_type_v = has_allocator_type<T>::value;

template <typename T, class = void>
struct is_vector : public std::false_type {};

template <typename T>
struct is_vector<
    T, std::enable_if_t<
           has_value_type_v<T> && has_allocator_type_v<T> &&
           std::is_same_v<T, std::vector<typename T::value_type,
                                         typename T::allocator_type>>>>
    : public std::true_type {};

template <typename T>
constexpr bool is_vector_v = is_vector<T>::value;

template <typename _Distribution, bool sample_dummy = false>
class rcpp_distribution_caller {
  using dist_t = _Distribution;
  using parm_t = typename dist_t::param_type;

  static constexpr bool is_multivariate_v =
      is_vector_v<typename dist_t::result_type>;

  template <typename _T, class = void>
  struct rtype : public std::integral_constant<int, -1> {};  // not implemented

  template <typename _T>
  struct rtype<_T, std::enable_if_t<std::is_same_v<bool, std::remove_cv_t<_T>>>>
      : public std::integral_constant<int, 12> {};  // LGLSXP

  template <typename _T>
  struct rtype<_T,
               std::enable_if_t<std::is_integral_v<_T> &&
                                !std::is_same_v<bool, std::remove_cv_t<_T>>>>
      : public std::integral_constant<int, 13> {};  // INTSXP

  template <typename _T>
  struct rtype<_T, std::enable_if_t<std::is_floating_point_v<_T>>>
      : public std::integral_constant<int, 14> {};  // REALSXP

  template <typename _T>
  static constexpr int rtype_v = rtype<_T>::value;

  template <typename _T, class = void>
  struct __value_type {
    using type = _T;
  };

  template <typename _T>
  struct __value_type<_T, std::enable_if_t<has_value_type_v<_T>>> {
    using type = typename _T::value_type;
  };

  using value_type = typename __value_type<typename dist_t::result_type>::type;

  using result_type =
      std::conditional_t<is_multivariate_v, Rcpp::Matrix<rtype_v<value_type>>,
                         Rcpp::Vector<rtype_v<value_type>>>;

  static constexpr std::size_t __dim(const parm_t& parm) {
    if constexpr (is_multivariate_v) {
      return parm.dim();
    } else {
      return std::size_t{1};
    }
  }

  static constexpr result_type __init(const std::size_t n,
                                      const std::size_t d) {
    if constexpr (is_multivariate_v) {
      return result_type(Rcpp::no_init(n, d));  // result_type == NumericMatrix
    } else {
      return result_type(Rcpp::no_init(n));  // result_type == NumericVector
    }
  }

 public:
  template <typename _Engine, typename... Args>
  static result_type call(_Engine&& engine, const std::size_t n,
                          Args&&... args) {
    using dist_t = _Distribution;
    using parm_t = typename dist_t::param_type;
    auto dist = dist_t{};
    const auto parm = parm_t{std::forward<decltype(args)>(args)...};

    const auto d = __dim(parm);
    auto out = __init(n, d);

    for (auto k = R_xlen_t{0}; k < n; ++k) {
      if constexpr (is_multivariate_v) {
        if ((d * k) % C_CHECK_USR_INTERRUP == 0) Rcpp::checkUserInterrupt();
        auto values = out(k, Rcpp::_);
        dist(engine, parm, values);
      } else {
        if (k % C_CHECK_USR_INTERRUP == 0) Rcpp::checkUserInterrupt();
        out[k] = dist(engine, parm);
      }

      if constexpr (sample_dummy)
        [[maybe_unused]] auto dummy = ::R_unif_index(1.);
    }
    return out;
  }
};
