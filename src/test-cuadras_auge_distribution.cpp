#include <algorithm>
#include <cmath>
#include <functional>

#include <Rcpp.h>
#include <r_engine.hpp>
#include <rmolib/random/multivariate/cuadras_auge_distribution.hpp>
#include <rmolib/random/univariate/exponential_distribution.hpp>
#include <testthat.h>

#include "testutils-approxequals.h"

#define RMO_TEST_DIST_NAME cuadras_auge_distribution
#define RMO_TEST_DIST_NAME_STRING "cuadras_auge_distribution"

#define RMO_TEST_ARG_LIST                                  \
  {                                                        \
    param_type{}, param_type{std::size_t{2}, 1., 0.},      \
        param_type{std::size_t{2}, 0., 1.},                \
        param_type{std::size_t{3}, 0.4, 0.2}, param_type { \
      std::size_t{3}, 0.7, 0.1                             \
    }                                                      \
  }

#define RMO_TEST_CHECK_PARAMS(__DIST__, __PARAMS__)    \
  expect_true(__DIST__.dim() == __PARAMS__.dim());     \
  expect_true(__DIST__.alpha() == __PARAMS__.alpha()); \
  expect_true(__DIST__.beta() == __PARAMS__.beta());

using exponential_distribution =
    rmolib::random::exponential_distribution<double>;
using cuadras_auge_distribution = rmolib::random::cuadras_auge_distribution<
    std::vector<double>, exponential_distribution>;
using param_type = cuadras_auge_distribution::param_type;

class generic_param_type {
 public:
  // compiler generated ctor and assignment op is sufficient

  template <typename _InputIterator>
  explicit generic_param_type(std::size_t dim, double alpha, double beta)
      : dim_{dim}, alpha_{alpha}, beta_{beta} {}

  template <typename _CuadrasAugeParamType,
            typename std::enable_if<
                !std::is_convertible_v<_CuadrasAugeParamType, generic_param_type> &&
                    rmolib::random::is_cuadras_auge_param_type_v<_CuadrasAugeParamType>,
                int>::type = 0>
  explicit generic_param_type(_CuadrasAugeParamType&& parm)
      : dim_{parm.dim()}, alpha_{parm.alpha()}, beta_{parm.beta()} {}

  auto dim() const { return dim_; }
  double alpha() const { return alpha_; };
  double beta() const { return beta_; }

 private:
  std::size_t dim_{1};
  double alpha_{1};
  double beta_{0};
};

#include "test-distribution.h"
