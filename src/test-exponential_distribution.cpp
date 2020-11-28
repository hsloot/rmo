#include <limits>

#include <r_engine.hpp>
#include <rmolib/random/univariate/exponential_distribution.hpp>
#include <testthat.h>

#define RMO_TEST_DIST_NAME exponential_dist_t
#define RMO_TEST_DIST_NAME_STRING "exponential_distribution"

#define RMO_TEST_ARG_LIST                                        \
  {                                                              \
    generic_parm_t{1.}, generic_parm_t{.5}, generic_parm_t{0.},  \
        generic_parm_t{std::numeric_limits<double>::infinity()}, \
        generic_parm_t {                                         \
      2.                                                         \
    }                                                            \
  }

#define RMO_TEST_CHECK_PARAMS(__DIST__, __PARAMS__) \
  expect_true(__DIST__.lambda() == __PARAMS__.lambda());

using exponential_dist_t = rmolib::random::exponential_distribution<double>;
using parm_t = exponential_dist_t::param_type;

class generic_param_type {
 public:
  generic_param_type() = default;

  explicit generic_param_type(const double lambda) : lambda_{lambda} {}

  template <
      typename _ExponentialParamType,
      typename std::enable_if<
          !std::is_convertible_v<_ExponentialParamType, generic_param_type> &&
              rmolib::random::is_exponential_param_type_v<
                  _ExponentialParamType>,
          int>::type = 0>
  explicit generic_param_type(_ExponentialParamType&& param)
      : lambda_{param.lambda()} {}

  // compiler generated ctor and assignment op is sufficient

  auto lambda() const { return lambda_; }

 private:
  double lambda_{1};
};
using generic_parm_t = generic_param_type;

#include "test-distribution.h"
