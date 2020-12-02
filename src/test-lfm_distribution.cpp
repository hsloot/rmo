#include <algorithm>
#include <cmath>
#include <functional>

#include <Rcpp.h>
#include <r_engine.hpp>
#include <rmolib/random/multivariate/lfm_distribution.hpp>
#include <rmolib/random/univariate/exponential_distribution.hpp>
#include <rmolib/random/univariate/pareto_distribution.hpp>  // (use pareto here)
#include <testthat.h>

#include "testutils-approxequals.h"
#include "testutils-tester_distribution.h"

using exponential_dist_t = rmolib::random::exponential_distribution<double>;
using uniform_real_dist_t = rmolib::random::uniform_real_distribution<double>;
using pareto_dist_t =
    rmolib::random::pareto_distribution<double, uniform_real_dist_t>;
using lfm_dist_t =
    rmolib::random::lfm_distribution<double, pareto_dist_t, exponential_dist_t>;
using jump_parm_t = pareto_dist_t::param_type;
using parm_t = lfm_dist_t::param_type;

namespace test_pareto_lfm_distribution {

class generic_param_type {
 public:
  generic_param_type() = default;

  explicit generic_param_type(const std::size_t dim, const double killing,
                              const double drift, const double intensity,
                              const jump_parm_t& jump_parm)
      : dim_{dim},
        killing_{killing},
        drift_{drift},
        intensity_{intensity},
        jump_parm_{jump_parm} {}

  template <typename _LFMParamType,
            typename std::enable_if<
                !std::is_convertible_v<_LFMParamType, generic_param_type> &&
                    rmolib::random::is_lfm_param_type_v<_LFMParamType>,
                int>::type = 0>
  explicit generic_param_type(_LFMParamType&& parm)
      : generic_param_type{parm.dim(), parm.killling(), parm.drift(),
                           parm.intensity(), parm.jump_parm()} {}

  // compiler generated ctor and assignment op is sufficient

  auto dim() const { return dim_; }
  auto killing() const { return killing_; }
  auto drift() const { return drift_; }
  auto intensity() const { return intensity_; }
  auto jump_param() const { return jump_parm_; }

 private:
  std::size_t dim_{1};
  double killing_{0};
  double drift_{0};
  double intensity_{1};
  jump_parm_t jump_parm_{};
};

}  // namespace test_pareto_lfm_distribution

using generic_parm_t = test_pareto_lfm_distribution::generic_param_type;

template <typename lfm_dist_t, typename generic_parm_t>
void tester_distribution<lfm_dist_t, generic_parm_t>::__param_test(
    const generic_param_type& test_parm) const {
  const auto dist = distribution_type{test_parm};
  expect_true(dist.dim() == test_parm.dim());
  expect_true(dist.killing() == test_parm.killing());
  expect_true(dist.drift() == test_parm.drift());
  expect_true(dist.intensity() == test_parm.intensity());
  expect_true(dist.jump_param() == test_parm.jump_param());
}

using dist_tester_t = tester_distribution<lfm_dist_t, generic_parm_t>;

context("pareto_lfm_distribution") {
  const auto test_cases = {
      generic_parm_t{},
      generic_parm_t{std::size_t{2}, 0., 0., 1., jump_parm_t{.5, 0.005}}};
  auto dist_tester = dist_tester_t{"pareto_lfm_distribution", test_cases};
  dist_tester.run_tests(r_engine{});
}
