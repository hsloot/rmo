#include <algorithm>
#include <cmath>
#include <functional>

#include <Rcpp.h>
#include <r_engine.hpp>
#include <rmolib/random/esm_mo_distribution.hpp>
#include <testthat.h>

#include "testutils-approxequals.h"

#define RMO_TEST_DIST_NAME esm_mo_distribution
#define RMO_TEST_DIST_NAME_STRING "esm_mo_distribution"

#define RMO_TEST_ARG_LIST                                                  \
  {                                                                        \
    param_type{}, param_type{std::size_t{2}, {1., 1., 1.}},                \
        param_type{std::size_t{3}, {0., 1., 2., 4., 5., 6.}}, param_type { \
      std::size_t{3}, { 2., 1., 0.5, 0.2, 0.3, 4., .7 }                    \
    }                                                                      \
  }

#define RMO_TEST_CHECK_PARAMS(__DIST__, __PARAMS__) \
  expect_true(__DIST__.dim() == __PARAMS__.dim());  \
  CATCH_CHECK_THAT(__DIST__.intensities(),          \
                   EqualsApprox(__PARAMS__.intensities()));

using exponential_distribution =
    rmolib::random::exponential_distribution<double>;
using esm_mo_distribution =
    rmolib::random::esm_mo_distribution<std::vector<double>,
                                        exponential_distribution>;
using param_type = esm_mo_distribution::param_type;

class generic_param_type {
 public:
  // compiler generated ctor and assignment op is sufficient

  template <typename _InputIterator>
  explicit generic_param_type(std::size_t dim, _InputIterator first,
                              _InputIterator last)
      : dim_{dim}, intensities_{first, last} {}

  template <typename _MOParamType,
            typename std::enable_if<
                !std::is_convertible_v<_MOParamType, generic_param_type> &&
                    rmolib::random::is_mo_param_type_v<_MOParamType>,
                int>::type = 0>
  explicit generic_param_type(_MOParamType&& parm)
      : dim_{parm.dim()}, intensities_{parm.intensities()} {}

  std::size_t dim() const { return dim_; }
  std::vector<double> intensities() const { return intensities_; }

 private:
  std::size_t dim_;
  std::vector<double> intensities_;
};

#include "test-real_distribution.h"
