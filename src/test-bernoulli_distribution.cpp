#include <algorithm>
#include <cmath>
#include <cstddef>
#include <functional>
#include <iterator>
#include <numeric>

#include <testthat.h>

// clang-format off
#include "rmolib/random/r_engine.hpp" // must be included before <rmolib/*>
// clang-format on
#include "rmolib/random/univariate/bernoulli_distribution.hpp"
#include "rmolib/random/univariate/uniform_real_distribution.hpp"
#include "rmolib/random/univariate/uniform_int_distribution.hpp"

#include "testutils-approxequals.h"
#include "testutils-tester_distribution.h"

using uniform_real_dist_t = rmolib::random::uniform_real_distribution<double>;
using bernoulli_dist_t =
    rmolib::random::bernoulli_distribution<int, double, uniform_real_dist_t>;
using parm_t = bernoulli_dist_t::param_type;

namespace test_bernoulli_distribution {

class generic_param_type {
 public:
  generic_param_type() = default;

  explicit generic_param_type(const double p) : p_{p} {}

  template <
      typename _BernoulliParamType,
      typename std::enable_if<
          !std::is_convertible_v<_BernoulliParamType, generic_param_type> &&
              rmolib::random::is_bernoulli_param_type_v<_BernoulliParamType>,
          int>::type = 0>
  explicit generic_param_type(_BernoulliParamType&& parm) : p_{parm.p()} {}

  auto p() const { return p_; }

  // compiler generated ctor and assignment op is sufficient

 private:
  double p_ = 1.;
};

}  // namespace test_bernoulli_distribution

using generic_parm_t = test_bernoulli_distribution::generic_param_type;

template <typename bernoulli_dist_t, typename generic_parm_t>
void tester_distribution<bernoulli_dist_t, generic_parm_t>::__param_test(
    const generic_param_type& test_parm) const {
  const auto dist = distribution_type{test_parm};
  expect_true(dist.p() == Approx(test_parm.p()));
}

using dist_tester_t = tester_distribution<bernoulli_dist_t, generic_parm_t>;

context("bernoulli_distribution") {
  const std::vector<generic_parm_t> test_cases = {
      generic_parm_t{}, generic_parm_t{0.}, generic_parm_t{1.},
      generic_parm_t{0.3}};
  auto dist_tester = dist_tester_t{"bernoulli_distribution", test_cases};
  dist_tester.run_tests(r_engine{});
}
