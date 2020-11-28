#include <r_engine.hpp>
#include <rmolib/random/univariate/uniform_int_distribution.hpp>
#include <testthat.h>

#define RMO_TEST_DIST_NAME uniform_int_dist_t
#define RMO_TEST_DIST_NAME_STRING "uniform_int_distribution"

#define RMO_TEST_ARG_LIST                                           \
  {                                                                 \
    generic_parm_t{}, generic_parm_t{0, 1}, generic_parm_t{0, 100}, \
        generic_parm_t{-100, 0}, generic_parm_t {                   \
      -100, 100                                                     \
    }                                                               \
  }

#define RMO_TEST_CHECK_PARAMS(__DIST__, __PARAMS__)    \
  expect_true(__DIST__.lower() == __PARAMS__.lower()); \
  expect_true(__DIST__.upper() == __PARAMS__.upper());

using uniform_int_dist_t = rmolib::random::uniform_int_distribution<int>;
using parm_t = uniform_int_dist_t::param_type;

class generic_param_type {
 public:
  generic_param_type() = default;

  explicit generic_param_type(const int lower, const int upper)
      : lower_{lower}, upper_{upper} {}

  template <
      typename _UniformIntParamType,
      typename std::enable_if<
          !std::is_convertible_v<_UniformIntParamType, generic_param_type> &&
              rmolib::random::is_uniform_int_param_type_v<_UniformIntParamType>,
          int>::type = 0>
  explicit generic_param_type(_UniformIntParamType&& param)
      : lower_{param.lower()}, upper_{param.upper()} {}

  // compiler generated ctor and assignment op is sufficient

  auto lower() const { return lower_; }
  auto upper() const { return upper_; }

 private:
  int lower_{0};
  int upper_{std::numeric_limits<int>::max()};
};
using generic_parm_t = generic_param_type;

#include "test-distribution.h"
