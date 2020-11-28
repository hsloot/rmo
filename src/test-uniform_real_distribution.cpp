#include <r_engine.hpp>
#include <rmolib/random/univariate/uniform_real_distribution.hpp>
#include <testthat.h>

#define RMO_TEST_DIST_NAME uniform_real_dist_t
#define RMO_TEST_DIST_NAME_STRING "uniform_real_distribution"

#define RMO_TEST_ARG_LIST                                             \
  {                                                                   \
    generic_parm_t{}, generic_parm_t{0., 1.}, generic_parm_t{0., 3.}, \
        generic_parm_t{-3., 0.}, generic_parm_t {                     \
      -1., 1.                                                         \
    }                                                                 \
  }

#define RMO_TEST_CHECK_PARAMS(__DIST__, __PARAMS__)    \
  expect_true(__DIST__.lower() == __PARAMS__.lower()); \
  expect_true(__DIST__.upper() == __PARAMS__.upper());

using uniform_real_dist_t = rmolib::random::uniform_real_distribution<double>;
using parm_t = uniform_real_dist_t::param_type;

class generic_param_type {
 public:
  generic_param_type() = default;

  explicit generic_param_type(const double lower, const double upper)
      : lower_{lower}, upper_{upper} {}

  template <
      typename _UniformParamType,
      typename std::enable_if<
          !std::is_convertible_v<_UniformParamType, generic_param_type> &&
              rmolib::random::is_uniform_real_param_type_v<_UniformParamType>,
          int>::type = 0>
  explicit generic_param_type(_UniformParamType&& param)
      : lower_{param.lower()}, upper_{param.upper()} {}

  // compiler generated ctor and assignment op is sufficient

  auto lower() const { return lower_; }
  auto upper() const { return upper_; }

 private:
  double lower_{0};
  double upper_{1};
};
using generic_parm_t = generic_param_type;

#include "test-distribution.h"
