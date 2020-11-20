#include <algorithm>
#include <cmath>
#include <functional>

#include <Rcpp.h>
#include <r_engine.hpp>
#include <rmolib/random/r_discrete_distribution.hpp>
#include <rmolib/random/uniform_real_distribution.hpp>
#include <testthat.h>

#include "testutils-approxequals.h"

#define RMO_TEST_DIST_NAME r_discrete_distribution
#define RMO_TEST_DIST_NAME_STRING "r_discrete_distribution"

#define RMO_TEST_ARG_LIST                                                      \
  {                                                                            \
    param_type{}, param_type{{1., 1., 1.}}, param_type{{0., 1., 2., 4.}},      \
        param_type{{2., 1., 0.5, 0.2}},                                        \
        param_type{{0.2, 0.3, 0.2, 0.1, 0.15, 0.05}}, param_type {             \
      7, 0.1, 0.5, [](auto&& val) { return std::forward<decltype(val)>(val); } \
    }                                                                          \
  }

#define RMO_TEST_CHECK_PARAMS(__DIST__, __PARAMS__) \
  CATCH_CHECK_THAT(__DIST__.probabilities(),        \
                   EqualsApprox(__PARAMS__.probabilities()));

using uniform_real_distribution =
    rmolib::random::uniform_real_distribution<double>;
using r_discrete_distribution =
    rmolib::random::r_discrete_distribution<int, double,
                                            uniform_real_distribution>;
using param_type = r_discrete_distribution::param_type;

class generic_param_type {
 public:
  // compiler generated ctor and assignment op is sufficient

  explicit generic_param_type(const std::vector<double>& p) : p_{p} {
    if (p_.begin() == p_.end()) {
      p_.clear();
      p_.emplace_back(1.);
      p_.shrink_to_fit();
    }
  }

  template <
      typename _DiscreteParamType,
      typename std::enable_if<
          !std::is_convertible_v<_DiscreteParamType, generic_param_type> &&
              rmolib::random::is_discrete_param_type_v<_DiscreteParamType>,
          int>::type = 0>
  explicit generic_param_type(_DiscreteParamType&& param)
      : p_{param.probabilities()} {}

  std::vector<double> probabilities() const { return p_; }

 private:
  std::vector<double> p_;
};

#include "test-distribution.h"
