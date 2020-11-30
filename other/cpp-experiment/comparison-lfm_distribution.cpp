// [[Rcpp::plugins(cpp17)]]
// [[Rcpp::depends(rmo)]]

#include <algorithm>
#include <random>

// clang-format off
#include <Rcpp.h>
#include <r_engine.hpp> // must be included before <rmolib/*>
// clang-format on

#include <rmo.hpp>
#include <rmolib/distribution.hpp>

using namespace Rcpp;

static const R_xlen_t C_CHECK_USR_INTERRUP = 100000;

// [[Rcpp::export]]
Rcpp::NumericMatrix sample_lfm_rmolib(const std::size_t n, const std::size_t d,
                                      const double killing, const double drift,
                                      const double intensity,
                                      const double alpha,
                                      const double lower_bound) {
  using dist_t = rmolib::lfm_pareto_distribution<double>;
  using parm_t = dist_t::param_type;

  r_engine engine{};
  dist_t dist{};
  parm_t parm(d, killing, drift, intensity, alpha, lower_bound);

  NumericMatrix out(no_init(n, d));
  for (R_xlen_t k = 0; k < n; ++k) {
    if ((d * k) % C_CHECK_USR_INTERRUP == 0) checkUserInterrupt();

    MatrixRow<REALSXP> values = out(k, _);
    dist(engine, parm, values);
  }
  return out;
}

// [[Rcpp::export]]
Rcpp::NumericMatrix sample_lfm_rmolib_copy(
    const std::size_t n, const std::size_t d, const double killing,
    const double drift, const double intensity, const double alpha,
    const double lower_bound) {
  using dist_t = rmolib::lfm_pareto_distribution<double>;
  using parm_t = dist_t::param_type;

  r_engine engine{};
  dist_t dist{};
  parm_t parm(d, killing, drift, intensity, alpha, lower_bound);

  NumericMatrix out(no_init(n, d));
  for (R_xlen_t k = 0; k < n; ++k) {
    if ((d * k) % C_CHECK_USR_INTERRUP == 0) checkUserInterrupt();

    MatrixRow<REALSXP> values = out(k, _);
    auto tmp = dist(engine, parm);
    std::move(tmp.begin(), tmp.end(), values.begin());
  }
  return out;
}


// [[Rcpp::export]]
Rcpp::NumericMatrix sample_lfm_old(const std::size_t n, const std::size_t d,
                                   const double killing, const double drift,
                                   const double intensity, const double alpha,
                                   const double lower_bound) {
  using mo::stats::RRNGPolicy;
  using RealUnivariateGenerator =
      mo::stats::RealUnivariateGenerator<double, RRNGPolicy>;
  using LFMCPPGenerator =
      mo::stats::LFMCPPGenerator<MatrixRow<REALSXP>, RRNGPolicy>;
  using ParetoGenerator = mo::stats::ParetoGenerator<RRNGPolicy>;

  std::unique_ptr<RealUnivariateGenerator> jump_generator =
      std::make_unique<ParetoGenerator>(alpha, lower_bound);
  LFMCPPGenerator lfm_cpp_generator(d, intensity, killing, drift,
                                    jump_generator);

  NumericMatrix out(no_init(n, d));
  for (R_xlen_t k = 0; k < n; ++k) {
    if ((d * k) % C_CHECK_USR_INTERRUP == 0) checkUserInterrupt();

    MatrixRow<REALSXP> values = out(k, _);
    lfm_cpp_generator(values);
  }
  return out;
}

/*** R
library(rmo)

use_seed <- 1623

n <- 1e3
d <- 10

killing <- 0.2
drift <- 0.1
intensity <- 1
alpha <- 0.5
x0 <- 5e-4

set.seed(1623)
sample_lfm_old(
  5, 10, killing, drift, intensity, alpha, x0)

set.seed(1623)
sample_lfm_rmolib(
  5, 10, killing, drift, intensity, alpha, x0)

set.seed(1623)
sample_lfm_rmolib_copy(
  5, 10, killing, drift, intensity, alpha, x0)

results <- bench::mark(
  LFM = sample_lfm_old(
    n, d, killing, drift, intensity, alpha, x0),
  LFM_rmolib = sample_lfm_rmolib(
    n, d, killing, drift, intensity, alpha, x0),
  LFM_rmolib_copy = sample_lfm_rmolib_copy(
    n, d, killing, drift, intensity, alpha, x0),
  min_iterations = 100L,
  check=FALSE)
ggplot2::autoplot(results)
*/
