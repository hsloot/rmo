#include <limits>

#include <r_engine.hpp>
#include <rmolib/random/univariate/deterministic_distribution.hpp>
#include <testthat.h>

#define RMO_TEST_DIST_NAME deterministic_dist_t
#define RMO_TEST_DIST_NAME_STRING "deterministic_distribution"

#define RMO_TEST_ARG_LIST                                        \
  {                                                              \
    generic_parm_t{1.}, generic_parm_t{.5}, generic_parm_t{0.},  \
        generic_parm_t{std::numeric_limits<double>::infinity()}, \
        generic_parm_t {                                         \
      2.                                                         \
    }                                                            \
  }

#define RMO_TEST_CHECK_PARAMS(__DIST__, __PARAMS__) \
  expect_true(__DIST__.value() == __PARAMS__.value());

using deterministic_dist_t = rmolib::random::deterministic_distribution<double>;
using parm_t = deterministic_dist_t::param_type;

class generic_param_type {
 public:
  generic_param_type() = default;

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

  // compiler generated ctor and assignment op is sufficient

  auto value() const { return value_; }

 private:
  double value_{1};
};
using generic_parm_t = generic_param_type;

#include "test-distribution.h"
