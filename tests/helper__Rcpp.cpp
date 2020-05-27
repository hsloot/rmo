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
IntegerVector Rcppmo_th_int(
    const R_xlen_t& n, const NumericVector& probabilities) {
  using IntGenerator = mo::stats::IntGenerator;
  using RIntGenerator = mo::stats::RIntGenerator;

  IntegerVector out(no_init(n));
  std::unique_ptr<IntGenerator> int_gen{new RIntGenerator(probabilities)};
  std::generate(out.begin(), out.end(), (*static_cast<RIntGenerator*>(int_gen.get())));

  return out;
}

// [[Rcpp::export]]
IntegerVector Rcppmo_th_perm(
    const R_xlen_t& n, const NumericVector& probabilities) {
  using SampleWalkerNoReplace = mo::stats::SampleWalkerNoReplace;
  using RSampleWalkerNoReplace = mo::stats::RSampleWalkerNoReplace;

  IntegerVector out(no_init(n));
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

// [[Rcpp::export]]
IntegerVector Rcppmo_th_sample_int(
    const R_xlen_t& n,
    const R_xlen_t& size,
    const bool& replace,
    const Nullable<NumericVector>& prob = R_NilValue,
    const bool& useHash = false) {
  using UnifIntGenerator = mo::stats::UnifIntGenerator;
  using IntGenerator = mo::stats::IntGenerator;
  using UnifSampleWalkerNoReplace = mo::stats::UnifSampleWalkerNoReplace;
  using SampleWalkerNoReplace = mo::stats::SampleWalkerNoReplace;
  using RUnifIntGenerator = mo::stats::RUnifIntGenerator;
  using RIntGenerator = mo::stats::RIntGenerator;
  using RUnifSampleWalkerNoReplace = mo::stats::RUnifSampleWalkerNoReplace;
  using RSampleWalkerNoReplace = mo::stats::RSampleWalkerNoReplace;

  if (useHash)
    std::logic_error("Function not yet implemented");
  auto flag_uniform = prob.isNull();
  IntegerVector out(no_init(size));
  if (replace) {
    if (flag_uniform) {
      std::unique_ptr<UnifIntGenerator> gen{new RUnifIntGenerator(n)};
      std::generate(out.begin(), out.end(), (*static_cast<RUnifIntGenerator*>(gen.get())));
    } else {
      NumericVector prob_(prob);
      R_xlen_t nc = 0;
      for (const auto& p : prob_)
        if (n * p >= 0.1)
          ++nc;
      if (nc > 200)
        std::logic_error("Function not yet implemented");

      std::unique_ptr<IntGenerator> gen{new RIntGenerator(prob_)};
      std::generate(out.begin(), out.end(), (*static_cast<RIntGenerator*>(gen.get())));
    }
  } else {
    if (flag_uniform) {
      std::unique_ptr<UnifSampleWalkerNoReplace> gen{new RUnifSampleWalkerNoReplace(n)};
      std::generate(out.begin(), out.end(), (*static_cast<RUnifSampleWalkerNoReplace*>(gen.get())));
    } else {
      NumericVector prob_(prob);
      std::unique_ptr<SampleWalkerNoReplace> gen{new RSampleWalkerNoReplace(prob_)};
      std::generate(out.begin(), out.end(), (*static_cast<RSampleWalkerNoReplace*>(gen.get())));
    }
  }

  return out;
}
