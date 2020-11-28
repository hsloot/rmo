// [[Rcpp::plugins(cpp17)]]
// [[Rcpp::depends(rmo)]]

#include <algorithm>
#include <random>

#include <Rcpp.h>
#include <r_engine.hpp>
#include <rmo.hpp>
#include <rmolib/distribution.hpp>

using namespace Rcpp;

static const R_xlen_t C_CHECK_USR_INTERRUP = 100000;

//' @keywords internal
//' @noRd
// [[Rcpp::export]]
NumericMatrix sample_arnold_rmolib(const std::size_t n, const std::size_t d,
                                   const NumericVector& intensities) {
  using arnold_mo_dist_t = rmolib::arnold_mo_distribution<double>;
  using parm_t = arnold_mo_dist_t::param_type;

  r_engine engine{};
  arnold_mo_dist_t dist{};
  parm_t parm(d, intensities.begin(), intensities.end());

  NumericMatrix out(no_init(n, d));
  for (R_xlen_t k = 0; k < n; k++) {
    if ((d * k) % C_CHECK_USR_INTERRUP == 0) checkUserInterrupt();

    MatrixRow<REALSXP> values = out(k, _);
    dist(engine, parm, values);
  }

  return out;
}

// [[Rcpp::export]]
NumericMatrix sample_arnold_rmolib_copy(
    const std::size_t n, const std::size_t d,
    const Rcpp::NumericVector& intensities) {
  using arnold_mo_dist_t = rmolib::arnold_mo_distribution<double>;
  using parm_t = arnold_mo_dist_t::param_type;

  r_engine engine{};
  arnold_mo_dist_t dist{};
  parm_t parm(d, intensities.begin(), intensities.end());

  NumericMatrix out(no_init(n, d));
  for (R_xlen_t k = 0; k < n; k++) {
    if ((d * k) % C_CHECK_USR_INTERRUP == 0) checkUserInterrupt();

    MatrixRow<REALSXP> values = out(k, _);
    auto tmp = dist(engine, parm);
    std::move(tmp.begin(), tmp.end(), values.begin());
  }
  return out;
}

// [[Rcpp::export]]
NumericMatrix sample_arnold_old(const std::size_t n, const std::size_t d,
                                const Rcpp::NumericVector& intensities) {
  mo::stats::ArnoldGenerator<MatrixRow<REALSXP>, mo::stats::RRNGPolicy>
      arnold_generator(d, intensities);

  NumericMatrix out(no_init(n, d));
  for (R_xlen_t k = 0; k < n; k++) {
    if ((d * k) % C_CHECK_USR_INTERRUP == 0) checkUserInterrupt();

    MatrixRow<REALSXP> values = out(k, _);
    arnold_generator(values);
  }

  return out;
}

/*** R
library(rmo)
n <- 1e3
d <- 10

lambda <- 1
alpha <- 0.5
x0 <- 5e-4
intensities <- lambda * intensities_pareto(d, alpha, x0)

results <- bench::mark(
  arnold = sample_arnold_old(
    n, d, intensities=intensities),
  arnold_rmolib = sample_arnold_rmolib(
    n, d, intensities=intensities),
  arnold_rmolib_copy = sample_arnold_rmolib_copy(
    n, d, intensities=intensities),
  min_iterations = 100L,
  check=FALSE)
ggplot2::autoplot(results)


lambda <- 1
eta <- 0.5
intensities <- intensities_poisson(d, lambda, eta)

results <- bench::mark(
  arnold = sample_arnold_old(
    n, d, intensities=intensities),
  arnold_rmolib = sample_arnold_rmolib(
    n, d, intensities=intensities),
  arnold_rmolib_copy = sample_arnold_rmolib_copy(
    n, d, intensities=intensities),
  min_iterations = 100L,
  check=FALSE)
ggplot2::autoplot(results)
*/
