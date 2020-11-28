#include <algorithm>
#include <cmath>
#include <functional>

#include <Rcpp.h>
#include <r_engine.hpp>
#include <rmolib/random/multivariate/cuadras_auge_distribution.hpp>
#include <rmolib/random/univariate/exponential_distribution.hpp>
#include <testthat.h>

#include "testutils-approxequals.h"

#define RMO_TEST_DIST_NAME cuadras_auge_dist_t
#define RMO_TEST_DIST_NAME_STRING "cuadras_auge_distribution"

#define RMO_TEST_ARG_LIST                                                     \
  {                                                                           \
    generic_parm_t{}, generic_parm_t{std::size_t{2}, 1., 0.}, generic_parm_t{std::size_t{2}, 0., 1.}, \
        generic_parm_t{std::size_t{3}, 0.4, 0.2}, generic_parm_t {                            \
      std::size_t{3}, 0.7, 0.1                                                \
    }                                                                         \
  }

#define RMO_TEST_CHECK_PARAMS(__DIST__, __PARAMS__)    \
  expect_true(__DIST__.dim() == __PARAMS__.dim());     \
  expect_true(__DIST__.alpha() == __PARAMS__.alpha()); \
  expect_true(__DIST__.beta() == __PARAMS__.beta());

using exponential_dist_t = rmolib::random::exponential_distribution<double>;
using cuadras_auge_dist_t =
    rmolib::random::cuadras_auge_distribution<double,
                                              exponential_dist_t>;
using parm_t = cuadras_auge_dist_t::param_type;

class generic_param_type {
 public:
  generic_param_type() = default;

  explicit generic_param_type(const std::size_t dim, const double alpha,
                              const double beta)
      : dim_{dim}, alpha_{alpha}, beta_{beta} {}

  template <
      typename _CuadrasAugeParamType,
      typename std::enable_if<
          !std::is_convertible_v<_CuadrasAugeParamType, generic_param_type> &&
              rmolib::random::is_cuadras_auge_param_type_v<
                  _CuadrasAugeParamType>,
          int>::type = 0>
  explicit generic_param_type(_CuadrasAugeParamType&& parm)
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
using generic_parm_t = generic_param_type;

#include "test-distribution.h"
