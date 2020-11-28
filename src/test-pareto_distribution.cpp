#include <algorithm>
#include <cmath>

#include <r_engine.hpp>
#include <rmolib/random/univariate/pareto_distribution.hpp>
#include <rmolib/random/univariate/uniform_real_distribution.hpp>
#include <testthat.h>

#define RMO_TEST_DIST_NAME pareto_dist_t
#define RMO_TEST_DIST_NAME_STRING "pareto_distribution"

#define RMO_TEST_ARG_LIST                                                \
  {                                                                      \
    generic_parm_t{}, generic_parm_t{1., 1.}, generic_parm_t{.5, 0.005}, \
        generic_parm_t {                                                 \
      2., 005                                                            \
    }                                                                    \
  }

#define RMO_TEST_CHECK_PARAMS(__DIST__, __PARAMS__)    \
  expect_true(__DIST__.alpha() == __PARAMS__.alpha()); \
  expect_true(__DIST__.lower_bound() == __PARAMS__.lower_bound());

using uniform_real_dist_t = rmolib::random::uniform_real_distribution<double>;
using pareto_dist_t =
    rmolib::random::pareto_distribution<double, uniform_real_dist_t>;
using parm_t = pareto_dist_t::param_type;

class generic_param_type {
 public:
  generic_param_type() = default;

  explicit generic_param_type(const double alpha, const double lower_bound)
      : alpha_{alpha}, lower_bound_{lower_bound} {}

  template <typename _ParetoParamType,
            typename std::enable_if<
                !std::is_convertible_v<_ParetoParamType, generic_param_type> &&
                    rmolib::random::is_pareto_param_type_v<_ParetoParamType>,
                int>::type = 0>
  explicit generic_param_type(_ParetoParamType&& param)
      : alpha_{param.alpha()}, lower_bound_{param.lower_bound()} {}

  // compiler generated ctor and assignment op is sufficient

  auto alpha() const { return alpha_; }
  auto lower_bound() const { return lower_bound_; }

 private:
  double alpha_{1.};
  double lower_bound_{1.};
};
using generic_parm_t = generic_param_type;

#include "test-distribution.h"
