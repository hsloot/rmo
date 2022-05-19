#include <limits>
#include <type_traits>
#include <vector>

#include <testthat.h>

// clang-format off
#include "rmolib/random/r_engine.hpp" // must be included before <rmolib/*>
// clang-format on
#include "rmolib/random/univariate/uniform_int_distribution.hpp"

#include "testutils-tester_distribution.h"

using uniform_int_dist_t = rmolib::random::uniform_int_distribution<int>;
using parm_t = uniform_int_dist_t::param_type;

namespace test_uniform_int_distribution {

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

}  // namespace test_uniform_int_distribution

using generic_parm_t = test_uniform_int_distribution::generic_param_type;

template <typename uniform_int_dist_t, typename generic_parm_t>
void tester_distribution<uniform_int_dist_t, generic_parm_t>::__param_test(
    const generic_param_type& test_parm) const {
  const auto dist = distribution_type{test_parm};
  expect_true(dist.lower() == test_parm.lower());
  expect_true(dist.upper() == test_parm.upper());
}

using dist_tester_t = tester_distribution<uniform_int_dist_t, generic_parm_t>;

context("uniform_int_distribution") {
  const std::vector<generic_parm_t> test_cases = {
      generic_parm_t{}, generic_parm_t{0, 1}, generic_parm_t{0, 100},
      generic_parm_t{-100, 0}, generic_parm_t{-100, 100}};
  auto dist_tester = dist_tester_t{"uniform_int_distribution", test_cases};
  dist_tester.run_tests(r_engine{});
}
