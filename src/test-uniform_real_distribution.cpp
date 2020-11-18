#include <r_engine.hpp>
#include <rmolib/random/uniform_real_distribution.hpp>
#include <testthat.h>

#define RMO_TEST_DIST_NAME uniform_real_distribution
#define RMO_TEST_DIST_NAME_STRING "uniform_real_distribution"

#define RMO_TEST_ARG_LIST                                                      \
  {                                                                            \
    param_type{}, param_type{0., 1.}, param_type{0., 3.}, param_type{-3., 0.}, \
        param_type {                                                           \
      -1., 1.                                                                  \
    }                                                                          \
  }

#define RMO_TEST_CHECK_PARAMS(__DIST__, __PARAMS__)    \
  expect_true(__DIST__.lower() == __PARAMS__.lower()); \
  expect_true(__DIST__.upper() == __PARAMS__.upper());

using uniform_real_distribution =
    rmolib::random::uniform_real_distribution<double>;
using param_type = uniform_real_distribution::param_type;

class generic_param_type {
 public:
  // compiler generated ctor and assignment op is sufficient

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

  double lower() const { return lower_; }
  double upper() const { return upper_; }

 private:
  double lower_{0.};
  double upper_{1.};
};

#include "test-real_distribution.h"
