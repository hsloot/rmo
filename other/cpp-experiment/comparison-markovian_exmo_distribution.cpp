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
NumericMatrix sample_markovian_rmolib(const R_xlen_t n, const int d,
                                      const NumericVector& ex_intensities) {
  using markovian_exmo_distribution =
      rmolib::markovian_exmo_distribution<std::vector<double>>;
  using param_type = markovian_exmo_distribution::param_type;

  r_engine engine{};
  markovian_exmo_distribution dist{};
  param_type parm(d, ex_intensities.begin(), ex_intensities.end());

  NumericMatrix out(no_init(n, d));
  for (R_xlen_t k = 0; k < n; k++) {
    if ((d * k) % C_CHECK_USR_INTERRUP == 0) checkUserInterrupt();

    MatrixRow<REALSXP> values = out(k, _);
    dist(engine, parm, values);
    // R's sample.int produces a final (redundant) selection of the
    // last remaining value see https://github.com/wch/r-source/blob/613bdfd0e1d3fc9984142d5da3da448adf2438c7/src/main/random.c#L461
    [[maybe_unused]] auto dummy = ::R_unif_index(1.);
  }

  return out;
}

// [[Rcpp::export]]
NumericMatrix sample_markovian_rmolib_copy(
    const int n, const std::size_t d,
    const Rcpp::NumericVector ex_intensities) {
  using markovian_exmo_distribution =
      rmolib::markovian_exmo_distribution<std::vector<double>>;
  using param_type = markovian_exmo_distribution::param_type;

  r_engine engine{};
  markovian_exmo_distribution dist{};
  param_type parm(d, ex_intensities.begin(), ex_intensities.end());

  NumericMatrix out(no_init(n, d));
  for (R_xlen_t k = 0; k < n; k++) {
    if ((d * k) % C_CHECK_USR_INTERRUP == 0) checkUserInterrupt();

    MatrixRow<REALSXP> values = out(k, _);
    auto tmp = dist(engine, parm);
    std::move(tmp.begin(), tmp.end(), values.begin());
    [[maybe_unused]] auto dummy = ::R_unif_index(1.);
  }
  return out;
}

// [[Rcpp::export]]
NumericMatrix sample_markovian_old(const int n, const std::size_t d,
                                   const Rcpp::NumericVector ex_intensities) {
  mo::stats::ExArnoldGenerator<MatrixRow<REALSXP>, mo::stats::RRNGPolicy> ex_arnold_generator(
      d, ex_intensities);

  NumericMatrix out(no_init(n, d));
  for (R_xlen_t k = 0; k < n; k++) {
    if ((d * k) % C_CHECK_USR_INTERRUP == 0) checkUserInterrupt();

    MatrixRow<REALSXP> values = out(k, _);
    ex_arnold_generator(values);
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
ex_intensities <- lambda * ex_intensities_pareto(d, alpha, x0)

use_seed <- 1623

set.seed(use_seed)
sample_markovian_old(5, d, ex_intensities=ex_intensities)

set.seed(use_seed)
sample_markovian_rmolib(5, d, ex_intensities=ex_intensities)

set.seed(use_seed)
sample_markovian_rmolib_copy(5, d, ex_intensities=ex_intensities)

results <- bench::mark(
  markovian = sample_markovian_old(
    n, d, ex_intensities=ex_intensities),
  markovian_rmolib = sample_markovian_rmolib(
    n, d, ex_intensities=ex_intensities),
  markovian_rmolib_copy = sample_markovian_rmolib_copy(
    n, d, ex_intensities=ex_intensities),
  min_iterations = 100L,
  check=FALSE)
ggplot2::autoplot(results)


lambda <- 1
eta <- 0.5
ex_intensities <- ex_intensities_poisson(d, lambda, eta)

results <- bench::mark(
  markovian = sample_markovian_old(
    n, d, ex_intensities=ex_intensities),
  markovian_rmolib = sample_markovian_rmolib(
    n, d, ex_intensities=ex_intensities),
  markovian_rmolib_copy = sample_markovian_rmolib_copy(
    n, d, ex_intensities=ex_intensities),
  min_iterations = 100L,
  check=FALSE)
ggplot2::autoplot(results)
*/
