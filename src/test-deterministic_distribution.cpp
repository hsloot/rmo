#include <limits>
#include <type_traits>
#include <vector>

#include <testthat.h>

// clang-format off
#include "rmolib/random/r_engine.hpp" // must be included before <rmolib/*>
// clang-format on
#include "rmolib/random/univariate/deterministic_distribution.hpp"

#include "testutils-tester_distribution.h"

using deterministic_dist_t = rmolib::random::deterministic_distribution<double>;
using parm_t = deterministic_dist_t::param_type;

namespace test_deterministic_distribution {

class generic_param_type {
   public:
    generic_param_type() = default;

    explicit generic_param_type(const double value) : value_{value} {}

    template <typename _DeterministicParamType,
              typename std::enable_if<
                  !std::is_convertible_v<_DeterministicParamType,
                                         generic_param_type> &&
                      rmolib::random::is_deterministic_param_type_v<
                          _DeterministicParamType>,
                  int>::type = 0>
    explicit generic_param_type(_DeterministicParamType&& param)
        : value_{param.value()} {}

    // compiler generated ctor and assignment op is sufficient

    auto value() const { return value_; }

   private:
    double value_{1};
};

}  // namespace test_deterministic_distribution

using generic_parm_t = test_deterministic_distribution::generic_param_type;

template <typename deterministic_dist_t, typename generic_parm_t>
void tester_distribution<deterministic_dist_t, generic_parm_t>::__param_test(
    const generic_param_type& test_parm) const {
    const auto dist = distribution_type{test_parm};
    expect_true(dist.value() == test_parm.value());
}

using dist_tester_t = tester_distribution<deterministic_dist_t, generic_parm_t>;

context("deterministic_distribution") {
    const std::vector<generic_parm_t> test_cases = {
        generic_parm_t{1.}, generic_parm_t{.5}, generic_parm_t{0.},
        generic_parm_t{std::numeric_limits<double>::infinity()},
        generic_parm_t{2.}};
    auto dist_tester = dist_tester_t{"deterministic_distribution", test_cases};
    dist_tester.run_tests(r_engine{});
}
