// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(rmo)]]

#include <Rcpp.h>
#include <mo.hpp>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector Rcppmo_th_rexp(
    const R_xlen_t& n, const double& rate=1.) {
  using RRNGPolicy = mo::stats::RRNGPolicy;
  using ExpGenerator = mo::stats::ExpGenerator<RRNGPolicy>;
  NumericVector out(no_init(n));

  std::unique_ptr<ExpGenerator> exp_generator{new ExpGenerator(rate)};
  std::generate(out.begin(), out.end(), (*static_cast<ExpGenerator*>(exp_generator.get())));

  return out;
}

// [[Rcpp::export]]
IntegerVector Rcppmo_th_int(
    const R_xlen_t& n, const NumericVector& probabilities) {
  using RRNGPolicy = mo::stats::RRNGPolicy;
  using CountGenerator = mo::stats::CountGenerator<RRNGPolicy>;

  IntegerVector out(no_init(n));
  std::unique_ptr<CountGenerator> count_generator{new CountGenerator(probabilities)};
  std::generate(out.begin(), out.end(), (*static_cast<CountGenerator*>(count_generator.get())));

  return out;
}

// [[Rcpp::export]]
IntegerVector Rcppmo_th_perm(
    const R_xlen_t& n, const NumericVector& probabilities) {
  using RRNGPolicy = mo::stats::RRNGPolicy;
  using SampleWalkerNoReplace = mo::stats::SampleWalkerNoReplace<RRNGPolicy>;

  IntegerVector out(no_init(n));
  std::unique_ptr<SampleWalkerNoReplace> sample_walker{new SampleWalkerNoReplace(probabilities)};
  std::generate(out.begin(), out.end(), (*static_cast<SampleWalkerNoReplace*>(sample_walker.get())));

  return out;
}

// [[Rcpp::export]]
NumericVector Rcppmo_th_fixeddbl(
    const R_xlen_t& n, const double& value) {
  using RRNGPolicy = mo::stats::RRNGPolicy;
  using FixedDblGenerator = mo::stats::FixedDblGenerator<RRNGPolicy>;

  NumericVector out(no_init(n));
  std::unique_ptr<FixedDblGenerator> fixeddbl_generator{new FixedDblGenerator(value)};
  std::generate(out.begin(), out.end(), (*static_cast<FixedDblGenerator*>(fixeddbl_generator.get())));

  return out;
}

// [[Rcpp::export]]
IntegerVector Rcppmo_th_sample_int(
    const R_xlen_t& n,
    const R_xlen_t& size,
    const bool& replace,
    const Nullable<NumericVector>& prob = R_NilValue,
    const bool& useHash = false) {
  using RRNGPolicy = mo::stats::RRNGPolicy;
  using UnifCountGenerator = mo::stats::UnifCountGenerator<RRNGPolicy>;
  using CountGenerator = mo::stats::CountGenerator<RRNGPolicy>;
  using UnifSampleWalkerNoReplace = mo::stats::UnifSampleWalkerNoReplace<RRNGPolicy>;
  using SampleWalkerNoReplace = mo::stats::SampleWalkerNoReplace<RRNGPolicy>;

  if (useHash)
    std::logic_error("Function not yet implemented");
  auto flag_uniform = prob.isNull();
  IntegerVector out(no_init(size));
  if (replace) {
    if (flag_uniform) {
      std::unique_ptr<UnifCountGenerator> gen{new UnifCountGenerator(n)};
      std::generate(out.begin(), out.end(), (*static_cast<UnifCountGenerator*>(gen.get())));
    } else {
      NumericVector prob_(prob);
      R_xlen_t nc = 0;
      for (const auto& p : prob_)
        if (n * p >= 0.1)
          ++nc;
      if (nc > 200)
        std::logic_error("Function not yet implemented");

      std::unique_ptr<CountGenerator> gen{new CountGenerator(prob_)};
      std::generate(out.begin(), out.end(), (*static_cast<CountGenerator*>(gen.get())));
    }
  } else {
    if (flag_uniform) {
      std::unique_ptr<UnifSampleWalkerNoReplace> gen{new UnifSampleWalkerNoReplace(n)};
      std::generate(out.begin(), out.end(), (*static_cast<UnifSampleWalkerNoReplace*>(gen.get())));
    } else {
      NumericVector prob_(prob);
      std::unique_ptr<SampleWalkerNoReplace> gen{new SampleWalkerNoReplace(prob_)};
      std::generate(out.begin(), out.end(), (*static_cast<SampleWalkerNoReplace*>(gen.get())));
    }
  }

  return out;
}
