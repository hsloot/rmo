// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(rmo)]]

#include <Rcpp.h>
#include <mo.hpp>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector Rcppmo_th_rexp(const R_xlen_t& n, const double& rate=1.) {
  NumericVector out(no_init(n));
  std::unique_ptr<mo::stats::ExpGenerator> exp_gen{new mo::stats::RExpGenerator(rate)};
  std::generate(out.begin(), out.end(), (*static_cast<mo::stats::RExpGenerator*>(exp_gen.get())));

  return out;
}

// [[Rcpp::export]]
NumericVector Rcppmo_th_unif(const R_xlen_t& n) {
  NumericVector out(no_init(n));
  std::unique_ptr<mo::stats::UnifGenerator> unif01_gen{new mo::stats::RUnifGenerator01()};
  std::generate(out.begin(), out.end(), (*static_cast<mo::stats::RUnifGenerator01*>(unif01_gen.get())));

  return out;
}

// [[Rcpp::export]]
NumericVector Rcppmo_th_int(const R_xlen_t& n, const NumericVector& probabilities) {
  NumericVector out(no_init(n));
  std::unique_ptr<mo::stats::IntGenerator> int_gen{new mo::stats::RIntGenerator(probabilities)};
  std::generate(out.begin(), out.end(), (*static_cast<mo::stats::RIntGenerator*>(int_gen.get())));

  return out;
}

// [[Rcpp::export]]
NumericVector Rcppmo_th_perm(const R_xlen_t& n, const NumericVector& probabilities) {
  NumericVector out(no_init(n));
  std::unique_ptr<mo::stats::SampleWalkerNoReplace> int_gen{new mo::stats::RSampleWalkerNoReplace(probabilities)};
  std::generate(out.begin(), out.end(), (*static_cast<mo::stats::RSampleWalkerNoReplace*>(int_gen.get())));

  return out;
}

// [[Rcpp::export]]
NumericVector Rcppmo_th_fixeddbl(const R_xlen_t& n, const double& value) {
  NumericVector out(no_init(n));
  std::unique_ptr<mo::stats::FixedDblGenerator> fixeddbl_gen{new mo::stats::FixedDblGenerator(value)};
  std::generate(out.begin(), out.end(), (*static_cast<mo::stats::FixedDblGenerator*>(fixeddbl_gen.get())));

  return out;
}
