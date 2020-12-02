#include <r_engine.hpp>
#include <rmolib/random/univariate/uniform_real_distribution.hpp>
#include <testthat.h>

#include "testutils-tester_distribution.h"

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

template <typename uniform_real_dist_t, typename generic_parm_t>
void tester_distribution<uniform_real_dist_t, generic_parm_t>::__param_test(
    const generic_param_type& test_parm) const {
  const auto dist = distribution_type{test_parm};
  expect_true(dist.lower() == test_parm.lower());
  expect_true(dist.upper() == test_parm.upper());
}

using dist_tester_t = tester_distribution<uniform_real_dist_t, generic_parm_t>;

context("uniform_real_distribution") {
  const auto test_cases = {generic_parm_t{}, generic_parm_t{0., 1.},
                           generic_parm_t{0., 3.}, generic_parm_t{-3., 0.},
                           generic_parm_t{-1., 1.}};
  auto dist_tester =
      dist_tester_t{"uniform_real_distribution", test_cases};
  dist_tester.run_tests(r_engine{});
}
