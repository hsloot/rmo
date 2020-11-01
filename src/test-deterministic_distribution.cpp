#include <limits>

#include <r_engine.h>
#include <rmolib/random/deterministic_distribution.hpp>
#include <testthat.h>

#define RMO_TEST_DIST_NAME deterministic_distribution
#define RMO_TEST_DIST_NAME_STRING "deterministic_distribution"

#define RMO_TEST_ARG_LIST                                                 \
  {                                                                       \
    param_type{1.}, param_type{.5}, param_type{0.},                       \
        param_type{std::numeric_limits<double>::infinity()}, param_type { \
      2.                                                                  \
    }                                                                     \
  }

#define RMO_TEST_CHECK_PARAMS(__DIST__, __PARAMS__) \
  expect_true(__DIST__.value() == __PARAMS__.value());

using deterministic_distribution =
    rmolib::random::deterministic_distribution<double>;
using param_type = deterministic_distribution::param_type;

class generic_param_type {
 public:
  // compiler generated ctor and assignment op is sufficient

  explicit generic_param_type(const double value) : value_{value} {}

  template <
      typename _DeterministicParamType,
      typename std::enable_if<
          !std::is_convertible_v<_DeterministicParamType, generic_param_type> &&
              rmolib::random::is_deterministic_param_type_v<
                  _DeterministicParamType>,
          int>::type = 0>
  explicit generic_param_type(_DeterministicParamType&& param)
      : value_{param.value()} {}

  double value() const { return value_; }

 private:
  double value_{1};
};

#include "test-real_distribution.h"
