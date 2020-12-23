#include <algorithm>
#include <cmath>
#include <functional>
#include <iterator>
#include <numeric>

// clang-format off
#include <rmolib/random/r_engine.hpp> // must be included before <rmolib/*>
// clang-format on
#include <rmolib/random/univariate/discrete_distribution.hpp>
#include <rmolib/random/univariate/uniform_real_distribution.hpp>
#include <rmolib/random/univariate/uniform_int_distribution.hpp>
#include <testthat.h>

#include "testutils-approxequals.h"
#include "testutils-tester_distribution.h"

using uniform_real_dist_t = rmolib::random::uniform_real_distribution<double>;
using uniform_int_dist_t =
    rmolib::random::uniform_int_distribution<std::size_t>;
using discrete_dist_t =
    rmolib::random::discrete_distribution<int, double, uniform_real_dist_t,
                                          uniform_int_dist_t>;
using parm_t = discrete_dist_t::param_type;

namespace test_discrete_distribution {

class generic_param_type {
 public:
  generic_param_type() { __init_empty(); }

  template <typename _InputIterator>
  explicit generic_param_type(_InputIterator first, _InputIterator last) {
    __init(first, last);
  }

  generic_param_type(std::initializer_list<double> wl)
      : generic_param_type{wl.begin(), wl.end()} {}

  explicit generic_param_type(const std::vector<double>& p)
      : generic_param_type{p.begin(), p.end()} {}

  template <class _UnaryFunctor>
  explicit generic_param_type(std::size_t count, const double xmin,
                              const double xmax, _UnaryFunctor unary_op)
      : generic_param_type{parm_t{count, xmin, xmax, unary_op}} {}

  template <
      typename _DiscreteParamType,
      typename std::enable_if<
          !std::is_convertible_v<_DiscreteParamType, generic_param_type> &&
              rmolib::random::is_discrete_param_type_v<_DiscreteParamType>,
          int>::type = 0>
  explicit generic_param_type(_DiscreteParamType&& param)
      : p_{param.probabilities()} {}

  auto probabilities() const { return p_; }

  // compiler generated ctor and assignment op is sufficient

 private:
  std::vector<double> p_ = {1.};

  void __init_empty() {}

  template <typename _InputIterator>
  void __init(_InputIterator first, _InputIterator last) {
    if (first == last) {
      __init_empty();
    } else {
      using std::distance;
      using std::iterator_traits;
      p_.clear();
      if constexpr (std::is_base_of_v<std::forward_iterator_tag,
                                      typename iterator_traits<
                                          _InputIterator>::iterator_category>)
        p_.reserve(distance(first, last));
      for (auto it = first; it != last; ++it) p_.emplace_back(*it);
      p_.shrink_to_fit();
      std::transform(p_.cbegin(), p_.cend(), p_.begin(),
                     [mass = std::accumulate(p_.begin(), p_.end(), 0.)](
                         auto val) { return val / mass; });
    }
  }
};

}  // namespace test_discrete_distribution

using generic_parm_t = test_discrete_distribution::generic_param_type;

template <typename discrete_dist_t, typename generic_parm_t>
void tester_distribution<discrete_dist_t, generic_parm_t>::__param_test(
    const generic_param_type& test_parm) const {
  const auto dist = distribution_type{test_parm};
  CATCH_CHECK_THAT(dist.probabilities(),
                   EqualsApprox(test_parm.probabilities()));
}

using dist_tester_t = tester_distribution<discrete_dist_t, generic_parm_t>;

context("discrete_distribution") {
  const std::vector<generic_parm_t> test_cases = {
      generic_parm_t{},
      generic_parm_t{{1., 1., 1.}},
      generic_parm_t{{0., 1., 2., 4.}},
      generic_parm_t{{2., 1., 0.5, 0.2}},
      generic_parm_t{{0.2, 0.3, 0.2, 0.1, 0.15, 0.05}},
      generic_parm_t{7, 0.1, 0.5, [](auto&& val) {
                       return std::forward<decltype(val)>(val);
                     }}};
  auto dist_tester = dist_tester_t{"discrete_distribution", test_cases};
  dist_tester.run_tests(r_engine{});
}
