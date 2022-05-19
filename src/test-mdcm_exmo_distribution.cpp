#include <cstddef>
#include <utility>
#include <vector>

#include <testthat.h>

// clang-format off
#include "rmolib/random/r_engine.hpp" // must be included before <rmolib/*>
// clang-format on
#include "rmolib/random/multivariate/mdcm_exmo_distribution.hpp"
#include "rmolib/random/univariate/exponential_distribution.hpp"
#include "rmolib/random/univariate/r_discrete_distribution.hpp"
#include "rmolib/random/univariate/uniform_int_distribution.hpp"
#include "rmolib/random/univariate/uniform_real_distribution.hpp"
#include "rmolib/algorithm/r_shuffle.hpp"

#include "testutils-approxequals.h"
#include "testutils-tester_distribution.h"

using uniform_real_dist_t = rmolib::random::uniform_real_distribution<double>;
using uniform_int_dist_t =
    rmolib::random::uniform_int_distribution<std::size_t>;
using exponential_dist_t = rmolib::random::exponential_distribution<double>;
using r_discrete_dist_t =
    rmolib::random::r_discrete_distribution<std::size_t, double,
                                            uniform_real_dist_t>;
using shuffler_t = rmolib::algorithm::r_shuffler;
using markovian_exmo_dist_t =
    rmolib::random::mdcm_exmo_distribution<double, exponential_dist_t,
                                           uniform_int_dist_t,
                                           r_discrete_dist_t, shuffler_t>;
using parm_t = markovian_exmo_dist_t::param_type;

namespace test_mdcm_exmo_distribution {

class generic_param_type {
 public:
  generic_param_type() = default;

  template <typename _InputIterator>
  explicit generic_param_type(const std::size_t dim, _InputIterator first,
                              _InputIterator last)
      : dim_{dim}, ex_intensities_{first, last} {}

  generic_param_type(const std::size_t dim, std::initializer_list<double> wl)
      : generic_param_type{dim, wl.begin(), wl.end()} {}

  template <typename _ExMOParamType,
            std::enable_if_t<
                !std::is_convertible_v<_ExMOParamType, generic_param_type> &&
                    rmolib::random::is_exmo_param_type_v<_ExMOParamType>,
                int> = 0>
  explicit generic_param_type(_ExMOParamType&& parm)
      : dim_{parm.dim()}, ex_intensities_{parm.ex_intensities()} {}

  // compiler generated ctor and assignment op is sufficient

  auto dim() const { return dim_; }
  auto ex_intensities() const { return ex_intensities_; }

 private:
  std::size_t dim_{1};
  std::vector<double> ex_intensities_ = {1.};
};

}  // namespace test_mdcm_exmo_distribution

using generic_parm_t = test_mdcm_exmo_distribution::generic_param_type;

template <typename markovian_exmo_dist_t, typename generic_parm_t>
void tester_distribution<markovian_exmo_dist_t, generic_parm_t>::__param_test(
    const generic_param_type& test_parm) const {
  const auto dist = distribution_type{test_parm};
  expect_true(dist.dim() == test_parm.dim());
  CATCH_CHECK_THAT(dist.ex_intensities(),
                   EqualsApprox(test_parm.ex_intensities()));
}

using dist_tester_t =
    tester_distribution<markovian_exmo_dist_t, generic_parm_t>;

context("mdcm_exmo_distribution") {
  const std::vector<generic_parm_t> test_cases = {
      generic_parm_t{}, generic_parm_t{std::size_t{3}, {1., 1., 1.}},
      generic_parm_t{std::size_t{6}, {0., 1., 2., 4., 5., 6.}},
      generic_parm_t{std::size_t{6}, {2., 1., 0.5, 0.2, 0.3, 4.}}};
  auto dist_tester = dist_tester_t{"mdcm_exmo_distribution", test_cases};
  dist_tester.run_tests(r_engine{});
}
