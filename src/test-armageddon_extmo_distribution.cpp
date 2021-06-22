#include <algorithm>
#include <cmath>
#include <functional>

// clang-format off
#include <rmolib/random/r_engine.hpp> // must be included before <rmolib/*>
// clang-format on
#include <rmolib/random/multivariate/armageddon_extmo_distribution.hpp>
#include <rmolib/random/univariate/exponential_distribution.hpp>
#include <testthat.h>

#include "testutils-approxequals.h"
#include "testutils-tester_distribution.h"

using exponential_dist_t = rmolib::random::exponential_distribution<double>;
using armageddon_extmo_dist_t =
    rmolib::random::armageddon_extmo_distribution<double, exponential_dist_t>;
using parm_t = armageddon_extmo_dist_t::param_type;

namespace test_armageddon_extmo_distribution {

class generic_param_type {
 public:
  generic_param_type() = default;

  explicit generic_param_type(const std::size_t dim, const double alpha,
                              const double beta)
      : dim_{dim}, alpha_{alpha}, beta_{beta} {}

  template <
      typename _ArmageddonExtMOParamType,
      typename std::enable_if<
          !std::is_convertible_v<_ArmageddonExtMOParamType, generic_param_type> &&
              rmolib::random::is_armageddon_extmo_param_type_v<
                  _ArmageddonExtMOParamType>,
          int>::type = 0>
  explicit generic_param_type(_ArmageddonExtMOParamType&& parm)
      : dim_{parm.dim()}, alpha_{parm.alpha()}, beta_{parm.beta()} {}

  // compiler generated ctor and assignment op is sufficient

  auto dim() const { return dim_; }
  auto alpha() const { return alpha_; };
  auto beta() const { return beta_; }

 private:
  std::size_t dim_{1};
  double alpha_{1};
  double beta_{0};
};

}  // namespace test_armageddon_extmo_distribution

using generic_parm_t = test_armageddon_extmo_distribution::generic_param_type;

template <typename armageddon_extmo_dist_t, typename generic_parm_t>
void tester_distribution<armageddon_extmo_dist_t, generic_parm_t>::__param_test(
    const generic_param_type& test_parm) const {
  const auto dist = distribution_type{test_parm};
  expect_true(dist.dim() == test_parm.dim());
  expect_true(dist.alpha() == test_parm.alpha());
  expect_true(dist.beta() == test_parm.beta());
}

using dist_tester_t = tester_distribution<armageddon_extmo_dist_t, generic_parm_t>;

context("armageddon_extmo_distribution") {
  const std::vector<generic_parm_t> test_cases = {
      generic_parm_t{}, generic_parm_t{std::size_t{2}, 1., 0.},
      generic_parm_t{std::size_t{2}, 0., 1.},
      generic_parm_t{std::size_t{3}, 0.4, 0.2},
      generic_parm_t{std::size_t{3}, 0.7, 0.1}};
  auto dist_tester = dist_tester_t{"armageddon_extmo_distribution", test_cases};
  dist_tester.run_tests(r_engine{});
}
