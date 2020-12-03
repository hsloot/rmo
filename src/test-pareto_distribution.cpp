#include <algorithm>
#include <cmath>

// clang-format off
#include <rmolib/random/r_engine.hpp> // must be included before <rmolib/*>
// clang-format on
#include <rmolib/random/univariate/pareto_distribution.hpp>
#include <rmolib/random/univariate/uniform_real_distribution.hpp>
#include <testthat.h>

#include "testutils-tester_distribution.h"

using uniform_real_dist_t = rmolib::random::uniform_real_distribution<double>;
using pareto_dist_t =
    rmolib::random::pareto_distribution<double, uniform_real_dist_t>;
using parm_t = pareto_dist_t::param_type;

namespace test_pareto_distribution {

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

}  // namespace test_pareto_distribution

using generic_parm_t = test_pareto_distribution::generic_param_type;

template <typename pareto_dist_t, typename generic_parm_t>
void tester_distribution<pareto_dist_t, generic_parm_t>::__param_test(
    const generic_param_type& test_parm) const {
  const auto dist = distribution_type{test_parm};
  expect_true(dist.alpha() == test_parm.alpha());
  expect_true(dist.lower_bound() == test_parm.lower_bound());
}

using dist_tester_t = tester_distribution<pareto_dist_t, generic_parm_t>;

context("pareto_distribution") {
  const std::vector<generic_parm_t> test_cases = {
      generic_parm_t{}, generic_parm_t{1., 1.}, generic_parm_t{.5, 0.005},
      generic_parm_t{2., 005}};
  auto dist_tester = dist_tester_t{"pareto_distribution", test_cases};
  dist_tester.run_tests(r_engine{});
}
