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

#define RMO_TEST_DIST_NAME markovian_exmo_distribution
#define RMO_TEST_DIST_NAME_STRING "markovian_exmo_distribution"

#define RMO_TEST_ARG_LIST                                                  \
  {                                                                        \
    param_type{}, param_type{std::size_t{3}, {1., 1., 1.}},                \
        param_type{std::size_t{6}, {0., 1., 2., 4., 5., 6.}}, param_type { \
      std::size_t{6}, { 2., 1., 0.5, 0.2, 0.3, 4., .7 }                    \
    }                                                                      \
  }

#define RMO_TEST_CHECK_PARAMS(__DIST__, __PARAMS__) \
  expect_true(__DIST__.dim() == __PARAMS__.dim());  \
  CATCH_CHECK_THAT(__DIST__.ex_intensities(),       \
                   EqualsApprox(__PARAMS__.ex_intensities()));

using uniform_real_distribution =
    rmolib::random::uniform_real_distribution<double>;
using uniform_int_distribution =
    rmolib::random::uniform_int_distribution<std::size_t>;
using exponential_distribution =
    rmolib::random::exponential_distribution<double>;
using r_discrete_distribution =
    rmolib::random::r_discrete_distribution<std::size_t, double,
                                            uniform_real_distribution>;
using markovian_exmo_distribution = rmolib::random::markovian_exmo_distribution<
    std::vector<double>, exponential_distribution, uniform_int_distribution,
    r_discrete_distribution>;
using param_type = markovian_exmo_distribution::param_type;

class generic_param_type {
 public:
  // compiler generated ctor and assignment op is sufficient

  template <typename _InputIterator>
  explicit generic_param_type(std::size_t dim, _InputIterator first,
                              _InputIterator last)
      : dim_{dim}, ex_intensities_{first, last} {}

  template <
      typename _ExMOParamType,
      std::enable_if_t<
          !std::is_convertible_v<_ExMOParamType, generic_param_type> &&
              rmolib::random::internal::is_exmo_param_type_v<_ExMOParamType>,
          int> = 0>
  explicit generic_param_type(_ExMOParamType&& parm)
      : dim_{parm.dim()}, ex_intensities_{parm.ex_intensities()} {}

  std::size_t dim() const { return dim_; }
  std::vector<double> ex_intensities() const { return ex_intensities_; }

 private:
  std::size_t dim_;
  std::vector<double> ex_intensities_;
};

#include "test-distribution.h"
