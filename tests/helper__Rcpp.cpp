// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(rmo)]]

#include <Rcpp.h>
#include <mo.hpp>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector Rcppmo_th_rexp(R_xlen_t n, double rate=1.) {
  NumericVector out(no_init(n));
  std::unique_ptr<mo::stats::ExpGenerator> exp_gen{new mo::stats::RExpGenerator(rate)};
  std::generate(out.begin(), out.end(), (*static_cast<mo::stats::RExpGenerator*>(exp_gen.get())));

  return out;
}

// [[Rcpp::export]]
NumericVector Rcppmo_th_unif(R_xlen_t n) {
  NumericVector out(no_init(n));
  std::unique_ptr<mo::stats::UnifGenerator> unif01_gen{new mo::stats::RUnifGenerator01()};
  std::generate(out.begin(), out.end(), (*static_cast<mo::stats::RUnifGenerator01*>(unif01_gen.get())));

  return out;
}

// [[Rcpp::export]]
NumericVector Rcppmo_th_int(R_xlen_t n, NumericVector probabilities) {
  NumericVector out(no_init(n));
  std::unique_ptr<mo::stats::IntGenerator> int_gen{new mo::stats::RIntGenerator(probabilities)};
  std::generate(out.begin(), out.end(), (*static_cast<mo::stats::RIntGenerator*>(int_gen.get())));

  return out;
}
