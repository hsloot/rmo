// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(rmo)]]

#include <Rcpp.h>
#include <mo.hpp>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector Rcppmo_th_rexp(
    const R_xlen_t& n, const double& rate=1.) {
  NumericVector out(no_init(n));
  using ExpGenerator = mo::stats::ExpGenerator;
  using RExpGenerator = mo::stats::RExpGenerator;

  std::unique_ptr<ExpGenerator> exp_gen{new RExpGenerator(rate)};
  std::generate(out.begin(), out.end(), (*static_cast<RExpGenerator*>(exp_gen.get())));

  return out;
}

// [[Rcpp::export]]
NumericVector Rcppmo_th_unif(
    const R_xlen_t& n) {
  using UnifGenerator = mo::stats::UnifGenerator;
  using RUnifGenerator01 = mo::stats::RUnifGenerator01;

  NumericVector out(no_init(n));
  std::unique_ptr<UnifGenerator> unif01_gen{new RUnifGenerator01()};
  std::generate(out.begin(), out.end(), (*static_cast<RUnifGenerator01*>(unif01_gen.get())));

  return out;
}

// [[Rcpp::export]]
NumericVector Rcppmo_th_int(
    const R_xlen_t& n, const NumericVector& probabilities) {
  using IntGenerator = mo::stats::IntGenerator;
  using RIntGenerator = mo::stats::RIntGenerator;

  NumericVector out(no_init(n));
  std::unique_ptr<IntGenerator> int_gen{new RIntGenerator(probabilities)};
  std::generate(out.begin(), out.end(), (*static_cast<RIntGenerator*>(int_gen.get())));

  return out;
}

// [[Rcpp::export]]
NumericVector Rcppmo_th_perm(
    const R_xlen_t& n, const NumericVector& probabilities) {
  using SampleWalkerNoReplace = mo::stats::SampleWalkerNoReplace;
  using RSampleWalkerNoReplace = mo::stats::RSampleWalkerNoReplace;

  NumericVector out(no_init(n));
  std::unique_ptr<SampleWalkerNoReplace> int_gen{new RSampleWalkerNoReplace(probabilities)};
  std::generate(out.begin(), out.end(), (*static_cast<RSampleWalkerNoReplace*>(int_gen.get())));

  return out;
}

// [[Rcpp::export]]
NumericVector Rcppmo_th_fixeddbl(
    const R_xlen_t& n, const double& value) {
  using FixedDblGenerator = mo::stats::FixedDblGenerator;

  NumericVector out(no_init(n));
  std::unique_ptr<FixedDblGenerator> fixeddbl_gen{new FixedDblGenerator(value)};
  std::generate(out.begin(), out.end(), (*static_cast<FixedDblGenerator*>(fixeddbl_gen.get())));

  return out;
}
