#include <algorithm>
#include <cmath>
#include <functional>
#include <iterator>

#include <Rcpp.h>
#include <r_engine.hpp>
#include <rmolib/random/univariate/r_discrete_distribution.hpp>
#include <rmolib/random/univariate/uniform_real_distribution.hpp>
#include <testthat.h>

#include "testutils-approxequals.h"

#define RMO_TEST_DIST_NAME r_discrete_dist_t
#define RMO_TEST_DIST_NAME_STRING "r_discrete_distribution"

#define RMO_TEST_ARG_LIST                                                      \
  {                                                                            \
    generic_parm_t{}, generic_parm_t{{1., 1., 1.}},                            \
        generic_parm_t{{0., 1., 2., 4.}}, generic_parm_t{{2., 1., 0.5, 0.2}},  \
        generic_parm_t{{0.2, 0.3, 0.2, 0.1, 0.15, 0.05}}, generic_parm_t {     \
      7, 0.1, 0.5, [](auto&& val) { return std::forward<decltype(val)>(val); } \
    }                                                                          \
  }

#define RMO_TEST_CHECK_PARAMS(__DIST__, __PARAMS__) \
  CATCH_CHECK_THAT(__DIST__.probabilities(),        \
                   EqualsApprox(__PARAMS__.probabilities()));

using uniform_real_dist_t = rmolib::random::uniform_real_distribution<double>;
using r_discrete_dist_t =
    rmolib::random::r_discrete_distribution<int, double, uniform_real_dist_t>;
using parm_t = r_discrete_dist_t::param_type;

class generic_param_type {
 public:
  generic_param_type() = default;

  template <typename _InputIterator>
  explicit generic_param_type(_InputIterator first, _InputIterator last) {
    __init(first, last);
  }

  generic_param_type(std::initializer_list<double> wl)
      : generic_param_type{wl.begin(), wl.end()} {}

  explicit generic_param_type(const std::vector<double>& p)
      : generic_param_type{p.begin(), p.end()} {}

  template <class _UnaryFunctor>
  explicit generic_param_type(std::size_t count, const double xmin,
                              const double xmax, _UnaryFunctor unary_op)
      : generic_param_type{parm_t{count, xmin, xmax, unary_op}} {}

  template <
      typename _DiscreteParamType,
      typename std::enable_if<
          !std::is_convertible_v<_DiscreteParamType, generic_param_type> &&
              rmolib::random::is_discrete_param_type_v<_DiscreteParamType>,
          int>::type = 0>
  explicit generic_param_type(_DiscreteParamType&& param)
      : p_{param.probabilities()} {}

  std::vector<double> probabilities() const { return p_; }

  // compiler generated ctor and assignment op is sufficient

 private:
  std::vector<double> p_ = {1.};

  void __init_empty() {}

  template <typename _InputIterator>
  void __init(_InputIterator first, _InputIterator last) {
    if (first == last) {
      __init_empty();
    } else {
      using std::distance;
      using std::iterator_traits;
      p_.clear();
      if constexpr (std::is_base_of_v<std::forward_iterator_tag,
                                      typename iterator_traits<
                                          _InputIterator>::iterator_category>)
        p_.reserve(distance(first, last));
      for (auto it = first; it != last; ++it) p_.emplace_back(*it);
      p_.shrink_to_fit();
      std::transform(p_.cbegin(), p_.cend(), p_.begin(),
                [mass = std:accumulate(p_.begin(), p_.end(), 0.)](auto val) {
                  return val / mass;
                });
    }
  }
};
using generic_parm_t = generic_param_type;

#include "test-distribution.h"
