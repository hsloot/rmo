#include <limits>

#include <r_engine.hpp>
#include <rmolib/random/exponential_distribution.hpp>
#include <testthat.h>

#define RMO_TEST_DIST_NAME exponential_distribution
#define RMO_TEST_DIST_NAME_STRING "exponential_distribution"

#define RMO_TEST_ARG_LIST                                                 \
  {                                                                       \
    param_type{1.}, param_type{.5}, param_type{0.},                       \
        param_type{std::numeric_limits<double>::infinity()}, param_type { \
      2.                                                                  \
    }                                                                     \
  }

#define RMO_TEST_CHECK_PARAMS(__DIST__, __PARAMS__) \
  expect_true(__DIST__.lambda() == __PARAMS__.lambda());

using exponential_distribution =
    rmolib::random::exponential_distribution<double>;
using param_type = exponential_distribution::param_type;

class generic_param_type {
 public:
  // compiler generated ctor and assignment op is sufficient

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

  double lambda() const { return lambda_; }

 private:
  double lambda_{1.};
};

#include "test-real_distribution.h"
