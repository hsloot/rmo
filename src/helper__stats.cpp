#include <Rcpp.h>
#include <rmo.hpp>

using namespace Rcpp;
using namespace mo::stats;

// [[Rcpp::export]]
NumericVector mo_internal__rexp(const R_xlen_t n, const double rate = 1.) {
  NumericVector out(no_init(n));

  ExpGenerator<RRNGPolicy> exp_generator(rate);
  std::generate(out.begin(), out.end(), exp_generator);

  return out;
}

// [[Rcpp::export]]
NumericVector mo_internal__rpareto(const R_xlen_t n, const double alpha,
                                   const double x0) {
  NumericVector out(no_init(n));

  ParetoGenerator<RRNGPolicy> pareto_generator(alpha, x0);
  std::generate(out.begin(), out.end(), pareto_generator);

  return out;
}

// [[Rcpp::export]]
NumericVector mo_internal__fixeddbl(const R_xlen_t n, const double value) {
  NumericVector out(no_init(n));
  FixedDblGenerator<RRNGPolicy> fixeddbl_generator(value);
  std::generate(out.begin(), out.end(), fixeddbl_generator);

  return out;
}

// [[Rcpp::export]]
IntegerVector mo_internal__count_replace(
    const R_xlen_t n, const R_xlen_t d,
    const Nullable<NumericVector> probabilities = R_NilValue) {
  IntegerVector out(no_init(n));
  if (probabilities.isNotNull()) {
    CountReplaceGenerator<RRNGPolicy> generator(
        as<NumericVector>(probabilities));
    std::generate(out.begin(), out.end(), generator);
  } else {
    UnifCountReplaceGenerator<RRNGPolicy> generator(d);
    std::generate(out.begin(), out.end(), generator);
  }

  return out;
}

// [[Rcpp::export]]
IntegerVector mo_internal__count_noreplace(
    const R_xlen_t n, const R_xlen_t d,
    const Nullable<NumericVector> probabilities = R_NilValue) {
  IntegerVector out(no_init(n));
  if (probabilities.isNotNull()) {
    CountNoReplaceWalker<RRNGPolicy> generator(
        as<NumericVector>(probabilities));
    std::generate(out.begin(), out.end(), generator);
  } else {
    UnifCountNoReplaceWalker<RRNGPolicy> generator(d);
    std::generate(out.begin(), out.end(), generator);
  }

  return out;
}

// [[Rcpp::export]]
IntegerVector mo_internal__perm(const R_xlen_t n) {
  IntegerVector out(no_init(n));

  UnifPermutationGenerator<IntegerVector, RRNGPolicy> perm_generator(n);

  return perm_generator();
}

// [[Rcpp::export]]
IntegerVector mo_internal__sample_int(
    const R_xlen_t n, const R_xlen_t size, const bool replace,
    const Nullable<NumericVector>& prob = R_NilValue,
    const bool useHash = false) {
  if (useHash) std::logic_error("Function not yet implemented");  // # nocov
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
        if (n * p >= 0.1) ++nc;
      if (nc > 200)
        std::logic_error("Function not yet implemented");  // # nocov

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
