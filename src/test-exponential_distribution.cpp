#include <limits>

#include <r_engine.hpp>
#include <rmolib/random/univariate/exponential_distribution.hpp>
#include <testthat.h>

#include "testutils-tester_distribution.h"

using exponential_dist_t = rmolib::random::exponential_distribution<double>;
using parm_t = exponential_dist_t::param_type;

namespace tests_exponential_distribution {

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

}  // namespace tests_exponential_distribution

using generic_parm_t = tests_exponential_distribution::generic_param_type;

template <typename exponential_dist_t, typename generic_parm_t>
void tester_distribution<exponential_dist_t, generic_parm_t>::__param_test(
    const generic_param_type& test_parm) const {
  const auto dist = distribution_type{test_parm};
  expect_true(dist.lambda() == test_parm.lambda());
}

using dist_tester_t = tester_distribution<exponential_dist_t, generic_parm_t>;

context("exponential_distribution") {
  auto test_cases = {
      generic_parm_t{1.}, generic_parm_t{.5}, generic_parm_t{0.},
      generic_parm_t{std::numeric_limits<double>::infinity()},
      generic_parm_t{2.}};
  auto dist_tester = dist_tester_t{"exponential_distribution", test_cases};
  dist_tester.run_tests(r_engine{});
}
