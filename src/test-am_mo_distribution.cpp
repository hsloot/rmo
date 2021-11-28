#include <algorithm>
#include <cmath>
#include <functional>

// clang-format off
#include <rmolib/random/r_engine.hpp> // must be included before <rmolib/*>
// clang-format on
#include <rmolib/random/multivariate/am_mo_distribution.hpp>
#include <rmolib/random/univariate/exponential_distribution.hpp>
#include <rmolib/random/univariate/r_discrete_distribution.hpp>
#include <rmolib/random/univariate/uniform_real_distribution.hpp>
#include <testthat.h>

#include "testutils-approxequals.h"
#include "testutils-tester_distribution.h"

using uniform_real_dist_t = rmolib::random::uniform_real_distribution<double>;
using exponential_dist_t = rmolib::random::exponential_distribution<double>;
using r_discrete_dist_t =
    rmolib::random::r_discrete_distribution<std::size_t, double,
                                            uniform_real_dist_t>;
using arnold_mo_dist_t =
    rmolib::random::am_mo_distribution<double, exponential_dist_t,
                                           r_discrete_dist_t>;
using parm_t = arnold_mo_dist_t::param_type;

namespace test_am_mo_distribution {

class generic_param_type {
 public:
  generic_param_type() = default;

  template <typename _InputIterator>
  explicit generic_param_type(const std::size_t dim, _InputIterator first,
                              _InputIterator last)
      : dim_{dim}, intensities_{first, last} {}

  generic_param_type(const std::size_t dim, std::initializer_list<double> wl)
      : generic_param_type{dim, wl.begin(), wl.end()} {}

  template <typename _MOParamType,
            typename std::enable_if<
                !std::is_convertible_v<_MOParamType, generic_param_type> &&
                    rmolib::random::is_mo_param_type_v<_MOParamType>,
                int>::type = 0>
  explicit generic_param_type(_MOParamType&& parm)
      : dim_{parm.dim()}, intensities_{parm.intensities()} {}

  // compiler generated ctor and assignment op is sufficient

  auto dim() const { return dim_; }
  auto intensities() const { return intensities_; }

 private:
  std::size_t dim_{1};
  std::vector<double> intensities_ = {1.};
};

}  // namespace test_am_mo_distribution

using generic_parm_t = test_am_mo_distribution::generic_param_type;

template <typename arnold_mo_dist_t, typename generic_parm_t>
void tester_distribution<arnold_mo_dist_t, generic_parm_t>::__param_test(
    const generic_param_type& test_parm) const {
  const auto dist = distribution_type{test_parm};
  expect_true(dist.dim() == test_parm.dim());
  CATCH_CHECK_THAT(dist.intensities(), EqualsApprox(test_parm.intensities()));
}

using dist_tester_t = tester_distribution<arnold_mo_dist_t, generic_parm_t>;

context("am_mo_distribution") {
  const std::vector<generic_parm_t> test_cases = {
      generic_parm_t{}, generic_parm_t{std::size_t{2}, {1., 1., 1.}},
      generic_parm_t{std::size_t{3}, {0., 1., 2., 3., 4., 5., 6.}},
      generic_parm_t{std::size_t{3}, {2., 1., 0.5, 0.2, 0.3, 4., .7}}};
  auto dist_tester = dist_tester_t{"am_mo_distribution", test_cases};
  dist_tester.run_tests(r_engine{});
}
