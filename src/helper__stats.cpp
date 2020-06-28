#include <Rcpp.h>

#include <rmo.hpp>

using namespace Rcpp;
using namespace mo::stats;

// [[Rcpp::export]]
NumericVector Rcppmo_th_rexp(
    const R_xlen_t n, const double rate=1.) {
  NumericVector out(no_init(n));

  ExpGenerator<RRNGPolicy> exp_generator(rate);
  std::generate(out.begin(), out.end(), exp_generator);

  return out;
}

// [[Rcpp::export]]
NumericVector Rcppmo_th_fixeddbl(
    const R_xlen_t n, const double value) {
  NumericVector out(no_init(n));
  FixedDblGenerator<RRNGPolicy> fixeddbl_generator(value);
  std::generate(out.begin(), out.end(), fixeddbl_generator);

  return out;
}

// [[Rcpp::export]]
IntegerVector Rcppmo_th_int(
    const R_xlen_t n, const NumericVector& probabilities) {
  IntegerVector out(no_init(n));
  CountReplaceGenerator<RRNGPolicy> count_generator(probabilities);
  std::generate(out.begin(), out.end(), count_generator);

  return out;
}

// [[Rcpp::export]]
IntegerVector Rcppmo_th_perm(
    const R_xlen_t n, const NumericVector& probabilities) {
  IntegerVector out(no_init(n));
  CountNoReplaceWalker<RRNGPolicy> sample_walker(probabilities);
  std::generate(out.begin(), out.end(), sample_walker);

  return out;
}

// [[Rcpp::export]]
IntegerVector Rcppmo_th_sample_int(
    const R_xlen_t n,
    const R_xlen_t size,
    const bool replace,
    const Nullable<NumericVector>& prob = R_NilValue,
    const bool useHash = false) {
  if (useHash)
    std::logic_error("Function not yet implemented");
  auto flag_uniform = prob.isNull();
  IntegerVector out(no_init(size));
  if (replace) {
    if (flag_uniform) {
      UnifCountReplaceGenerator<RRNGPolicy> gen(n);
      std::generate(out.begin(), out.end(), gen);
    } else {
      NumericVector prob_(prob);
      R_xlen_t nc = 0;
      for (const auto& p : prob_)
        if (n * p >= 0.1)
          ++nc;
      if (nc > 200)
        std::logic_error("Function not yet implemented");

      CountReplaceGenerator<RRNGPolicy> gen(prob_);
      std::generate(out.begin(), out.end(), gen);
    }
  } else {
    if (flag_uniform) {
      UnifCountNoReplaceWalker<RRNGPolicy> gen(n);
      std::generate(out.begin(), out.end(), gen);
    } else {
      NumericVector prob_(prob);
      CountNoReplaceWalker<RRNGPolicy> gen(prob_);
      std::generate(out.begin(), out.end(), gen);
    }
  }

  return out;
}
