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

//' @keywords internal
//' @noRd
// [[Rcpp::export]]
NumericMatrix sample_cuadras_auge_rmolib(const std::size_t n,
                                         const std::size_t d,
                                         const double alpha,
                                         const double beta) {
  using cuadras_auge_dist_t = rmolib::cuadras_auge_distribution<double>;
  using parm_t = cuadras_auge_dist_t::param_type;

  r_engine engine{};
  cuadras_auge_dist_t dist{};
  parm_t parm(d, alpha, beta);

  NumericMatrix out(no_init(n, d));
  for (R_xlen_t k = 0; k < n; k++) {
    if ((d * k) % C_CHECK_USR_INTERRUP == 0) checkUserInterrupt();

    MatrixRow<REALSXP> values = out(k, _);
    dist(engine, parm, values);
  }

  return out;
}

// [[Rcpp::export]]
NumericMatrix sample_cuadras_auge_rmolib_copy(const std::size_t n,
                                              const std::size_t d,
                                              const double alpha,
                                              const double beta) {
  using cuadras_auge_dist_t = rmolib::cuadras_auge_distribution<double>;
  using parm_t = cuadras_auge_dist_t::param_type;

  r_engine engine{};
  cuadras_auge_dist_t dist{};
  parm_t parm(d, alpha, beta);

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
NumericMatrix sample_cuadras_auge_old(const std::size_t n, const std::size_t d,
                                      const double alpha, const double beta) {
  mo::stats::CuadrasAugeGenerator<MatrixRow<REALSXP>, mo::stats::RRNGPolicy>
      cuadras_auge_generator(d, alpha, beta);

  NumericMatrix out(no_init(n, d));
  for (R_xlen_t k = 0; k < n; k++) {
    if ((d * k) % C_CHECK_USR_INTERRUP == 0) checkUserInterrupt();

    MatrixRow<REALSXP> values = out(k, _);
    cuadras_auge_generator(values);
  }

  return out;
}

/*** R
library(rmo)
n <- 1e3
d <- 10

alpha <- 0.6
beta <- 0.2

results <- bench::mark(
  cuadras_auge = sample_cuadras_auge_old(
    n, d, alpha=alpha, beta=beta),
  cuadras_auge_rmolib = sample_cuadras_auge_rmolib(
    n, d, alpha=alpha, beta=beta),
  cuadras_auge_rmolib_copy = sample_cuadras_auge_rmolib_copy(
    n, d, alpha=alpha, beta=beta),
  min_iterations = 100L,
  check=FALSE)
ggplot2::autoplot(results)
*/
