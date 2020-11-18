// [[Rcpp::plugins(cpp17)]]
// [[Rcpp::depends(rmo)]]

#include <random>
#include <algorithm>

#include <Rcpp.h>
#include <r_engine.hpp>
#include <rmo.hpp>
#include <rmolib/distribution.hpp>

using namespace Rcpp;

static const R_xlen_t C_CHECK_USR_INTERRUP = 100000;

// [[Rcpp::export]]
Rcpp::NumericMatrix sample_esm_rmolib(const int n, const std::size_t d,
                                      const Rcpp::NumericVector intensities) {
  using esm_mo_distribution = rmolib::esm_mo_distribution<std::vector<double>>;
  using param_type = esm_mo_distribution::param_type;

  r_engine engine{};
  esm_mo_distribution dist{};
  param_type parm(d, intensities.begin(), intensities.end());

  NumericMatrix out(no_init(n, d));
  for (R_xlen_t k = 0; k < n; k++) {
    if ((d * k) % C_CHECK_USR_INTERRUP == 0) checkUserInterrupt();

    MatrixRow<REALSXP> values = out(k, _);
    dist(engine, parm, values);
  }
  return out;
}


// [[Rcpp::export]]
Rcpp::NumericMatrix sample_esm_rmolib_copy(const int n, const std::size_t d,
                                      const Rcpp::NumericVector intensities) {
  using esm_mo_distribution = rmolib::esm_mo_distribution<std::vector<double>>;
  using param_type = esm_mo_distribution::param_type;

  r_engine engine{};
  esm_mo_distribution dist{};
  param_type parm(d, intensities.begin(), intensities.end());

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
Rcpp::NumericMatrix sample_esm_old(const int n, const std::size_t d,
                                   const Rcpp::NumericVector intensities) {
  mo::stats::ESMGenerator<MatrixRow<REALSXP>, mo::stats::RRNGPolicy>
      esm_generator(d, intensities);

  NumericMatrix out(no_init(n, d));
  for (R_xlen_t k = 0; k < n; k++) {
    if ((d * k) % C_CHECK_USR_INTERRUP == 0) checkUserInterrupt();

    MatrixRow<REALSXP> values = out(k, _);
    esm_generator(values);
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
  ESM = sample_esm_old(
    n, d, intensities=intensities),
  ESM_rmolib = sample_esm_rmolib(
    n, d, intensities=intensities),
  ESM_rmolib_copy = sample_esm_rmolib_copy(
    n, d, intensities=intensities),
  min_iterations = 100L,
  check=FALSE)
ggplot2::autoplot(results)


lambda <- 1
eta <- 0.5
intensities <- intensities_poisson(d, lambda, eta)

results <- bench::mark(
  ESM = sample_esm_old(
    n, d, intensities=intensities),
  ESM_rmolib = sample_esm_rmolib(
    n, d, intensities=intensities),
  ESM_rmolib_copy = sample_esm_rmolib_copy(
    n, d, intensities=intensities),
  min_iterations = 100L,
  check=FALSE)
ggplot2::autoplot(results)
*/
