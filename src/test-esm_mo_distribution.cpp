#include <algorithm>
#include <cmath>
#include <functional>

#include <Rcpp.h>
#include <r_engine.hpp>
#include <rmolib/random/multivariate/esm_mo_distribution.hpp>
#include <rmolib/random/univariate/exponential_distribution.hpp>
#include <testthat.h>

#include "testutils-approxequals.h"

#define RMO_TEST_DIST_NAME esm_mo_dist_t
#define RMO_TEST_DIST_NAME_STRING "esm_mo_distribution"

#define RMO_TEST_ARG_LIST                                             \
  {                                                                   \
    generic_parm_t{}, generic_parm_t{std::size_t{2}, {1., 1., 1.}},   \
        generic_parm_t{std::size_t{3}, {0., 1., 2., 3., 4., 5., 6.}}, \
        generic_parm_t {                                              \
      std::size_t{3}, { 2., 1., 0.5, 0.2, 0.3, 4., .7 }               \
    }                                                                 \
  }

#define RMO_TEST_CHECK_PARAMS(__DIST__, __PARAMS__) \
  expect_true(__DIST__.dim() == __PARAMS__.dim());  \
  CATCH_CHECK_THAT(__DIST__.intensities(),          \
                   EqualsApprox(__PARAMS__.intensities()));

using exponential_dist_t = rmolib::random::exponential_distribution<double>;
using esm_mo_dist_t =
    rmolib::random::esm_mo_distribution<double, exponential_dist_t>;
using parm_t = esm_mo_dist_t::param_type;

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
using generic_parm_t = generic_param_type;

#include "test-distribution.h"
