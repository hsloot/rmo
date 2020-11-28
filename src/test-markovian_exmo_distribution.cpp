#include <algorithm>
#include <cmath>
#include <functional>

#include <Rcpp.h>
#include <r_engine.hpp>
#include <rmolib/random/multivariate/markovian_exmo_distribution.hpp>
#include <rmolib/random/univariate/exponential_distribution.hpp>
#include <rmolib/random/univariate/r_discrete_distribution.hpp>
#include <rmolib/random/univariate/uniform_int_distribution.hpp>
#include <rmolib/random/univariate/uniform_real_distribution.hpp>
#include <testthat.h>

#include "testutils-approxequals.h"

#define RMO_TEST_DIST_NAME markovian_exmo_dist_t
#define RMO_TEST_DIST_NAME_STRING "markovian_exmo_distribution"

#define RMO_TEST_ARG_LIST                                           \
  {                                                                 \
    generic_parm_t{}, generic_parm_t{std::size_t{3}, {1., 1., 1.}}, \
        generic_parm_t{std::size_t{6}, {0., 1., 2., 4., 5., 6.}},   \
        generic_parm_t {                                            \
      std::size_t{6}, { 2., 1., 0.5, 0.2, 0.3, 4. }             \
    }                                                               \
  }

#define RMO_TEST_CHECK_PARAMS(__DIST__, __PARAMS__) \
  expect_true(__DIST__.dim() == __PARAMS__.dim());  \
  CATCH_CHECK_THAT(__DIST__.ex_intensities(),       \
                   EqualsApprox(__PARAMS__.ex_intensities()));

using uniform_real_dist_t = rmolib::random::uniform_real_distribution<double>;
using uniform_int_dist_t =
    rmolib::random::uniform_int_distribution<std::size_t>;
using exponential_dist_t = rmolib::random::exponential_distribution<double>;
using r_discrete_dist_t =
    rmolib::random::r_discrete_distribution<std::size_t, double,
                                            uniform_real_dist_t>;
using markovian_exmo_dist_t = rmolib::random::markovian_exmo_distribution<
    double, exponential_dist_t, uniform_int_dist_t, r_discrete_dist_t>;
using parm_t = markovian_exmo_dist_t::param_type;

class generic_param_type {
 public:
  generic_param_type() = default;

  template <typename _InputIterator>
  explicit generic_param_type(const std::size_t dim, _InputIterator first,
                              _InputIterator last)
      : dim_{dim}, ex_intensities_{first, last} {}

  generic_param_type(const std::size_t dim, std::initializer_list<double> wl)
      : generic_param_type{dim, wl.begin(), wl.end()} {}

  template <
      typename _ExMOParamType,
      std::enable_if_t<
          !std::is_convertible_v<_ExMOParamType, generic_param_type> &&
              rmolib::random::internal::is_exmo_param_type_v<_ExMOParamType>,
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
using generic_parm_t = generic_param_type;

#include "test-distribution.h"
