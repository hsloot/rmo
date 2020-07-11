#include <Rcpp.h>
#include <rmo.hpp>

static const R_xlen_t C_CHECK_USR_INTERRUP = 100000;

using namespace Rcpp;
using namespace mo::stats;

//' @keywords internal
//' @noRd
// [[Rcpp::export]]
NumericMatrix Rcpp__rmo_esm(const R_xlen_t n, const R_xlen_t d,
                            const NumericVector& intensities) {
  ESMGenerator<MatrixRow<REALSXP>, RRNGPolicy> esm_generator(d, intensities);

  NumericMatrix out(no_init(n, d));
  for (R_xlen_t k = 0; k < n; k++) {
    if ((d * k) % C_CHECK_USR_INTERRUP == 0) checkUserInterrupt();

    MatrixRow<REALSXP> values = out(k, _);
    esm_generator(values);
  }
  return out;
}

//' @keywords internal
//' @noRd
// [[Rcpp::export]]
NumericMatrix Rcpp__rmo_arnold(const R_xlen_t n, const int d,
                               const NumericVector& intensities) {
  ArnoldGenerator<MatrixRow<REALSXP>, RRNGPolicy> arnold_generator(d,
                                                                   intensities);

  NumericMatrix out(no_init(n, d));
  for (R_xlen_t k = 0; k < n; k++) {
    if ((d * k) % C_CHECK_USR_INTERRUP == 0) checkUserInterrupt();

    MatrixRow<REALSXP> values = out(k, _);
    arnold_generator(values);
  }

  return out;
}

//' @keywords internal
//' @noRd
// [[Rcpp::export]]
NumericMatrix Rcpp__rmo_ex_arnold(const R_xlen_t n, const int d,
                                  const NumericVector& ex_intensities) {
  ExArnoldGenerator<MatrixRow<REALSXP>, RRNGPolicy> ex_arnold_generator(
      d, ex_intensities);

  NumericMatrix out(no_init(n, d));
  for (R_xlen_t k = 0; k < n; k++) {
    if ((d * k) % C_CHECK_USR_INTERRUP == 0) checkUserInterrupt();

    MatrixRow<REALSXP> values = out(k, _);
    ex_arnold_generator(values);
  }
  return out;
}

//' @keywords internal
//' @noRd
// [[Rcpp::export]]
NumericMatrix Rcpp__rmo_esm_cuadras_auge(const R_xlen_t n, const int d,
                                         const double alpha,
                                         const double beta) {
  CuadrasAugeGenerator<MatrixRow<REALSXP>, RRNGPolicy> cuadras_auge_generator(
      d, alpha, beta);

  NumericMatrix out(no_init(n, d));
  for (R_xlen_t k = 0; k < n; k++) {
    if ((d * k) % C_CHECK_USR_INTERRUP == 0) checkUserInterrupt();

    MatrixRow<REALSXP> values = out(k, _);
    cuadras_auge_generator(values);
  }

  return out;
}

std::unique_ptr<RealUnivariateGenerator<double, RRNGPolicy>>
get_univariate_generator(const std::string name, const List& args);

//' @keywords internal
//' @noRd
// [[Rcpp::export]]
NumericMatrix Rcpp__rmo_lfm_cpp(const R_xlen_t n, const R_xlen_t d,
                                const double rate, const double rate_killing,
                                const double rate_drift,
                                const std::string rjump_name,
                                const List& rjump_arg_list) {
  std::unique_ptr<RealUnivariateGenerator<double, RRNGPolicy>> jump_generator =
      get_univariate_generator(rjump_name, rjump_arg_list);
  LFMCPPGenerator<MatrixRow<REALSXP>, RRNGPolicy> lfm_cpp_generator(
      d, rate, rate_killing, rate_drift, jump_generator);

  NumericMatrix out(no_init(n, d));
  for (R_xlen_t k = 0; k < n; k++) {
    if ((d * k) % C_CHECK_USR_INTERRUP == 0) checkUserInterrupt();

    MatrixRow<REALSXP> values = out(k, _);
    lfm_cpp_generator(values);
  }
  return out;
}

//' @keywords internal
//' @noRd
std::unique_ptr<RealUnivariateGenerator<double, RRNGPolicy>>
get_univariate_generator(const std::string name, const List& args) {
  std::unique_ptr<RealUnivariateGenerator<double, RRNGPolicy>> out;
  if ("rexp" == name) {
    double rate = args["rate"];
    out.reset(new ExpGenerator<RRNGPolicy>(rate));
  } else if ("rposval" == name) {
    double value = args["value"];
    out.reset(new FixedDblGenerator<RRNGPolicy>(value));
  } else if ("rpareto" == name) {  // #nocov start
    double alpha = args["alpha"];
    double x0 = args["x0"];
    out.reset(new ParetoGenerator<RRNGPolicy>(alpha, x0));
  } else {                            // # nocov end
    std::logic_error("wrong input");  // # nocov
  }

  return out;
}
