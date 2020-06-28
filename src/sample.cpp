#include <Rcpp.h>

#include <rmo.hpp>

static const R_xlen_t C_CHECK_USR_INTERRUP = 100000;

using namespace Rcpp;
using namespace mo::stats;


//' @keywords internal
//' @noRd
// [[Rcpp::export]]
NumericMatrix Rcpp__rmo_esm(
    const R_xlen_t n, const R_xlen_t d,
    const NumericVector& intensities) {
  auto num_shocks = intensities.size();
  if ((1<<d)-1 != num_shocks)
    std::range_error("intensities.size() != 2^d-1"); // # nocov

  std::unique_ptr<ExpGenerator<RRNGPolicy>> exp_generator{new ExpGenerator<RRNGPolicy>()};

  NumericMatrix out(no_init(n, d));
  std::fill(out.begin(), out.end(), R_PosInf);
  for (R_xlen_t k=0; k<n; k++) {
    if ((d*k) % C_CHECK_USR_INTERRUP == 0)
      checkUserInterrupt();

    MatrixRow<REALSXP> values = out(k, _);
    for (R_xlen_t j=0; j<num_shocks; j++) {
    // dont't use intensities.size() for performance
      if (intensities[j] > 0.) {
        auto shock_time = (*exp_generator)(intensities[j]);
        for (int i=0; i<d; i++) {
        // don't use values.size() for performance
          if (mo::math::is_within(i, j)) {
              values[i] = mo::math::min(values[i], shock_time);
          }
        }
      }
    }
  }
  return out;
}


//' @keywords internal
//' @noRd
// [[Rcpp::export]]
NumericMatrix Rcpp__rmo_arnold(
    const R_xlen_t n, const int d,
    const NumericVector& intensities) {
  auto total_intensity = sum(intensities);
  std::unique_ptr<ExpGenerator<RRNGPolicy>> exp_generator{new ExpGenerator<RRNGPolicy>(total_intensity)};
  std::unique_ptr<CountReplaceGenerator<RRNGPolicy>> count_generator{new CountReplaceGenerator<RRNGPolicy>(intensities)};

  NumericMatrix out(n, d);
  for (R_xlen_t k=0; k<n; k++) {
    if ((d*k) % C_CHECK_USR_INTERRUP == 0)
      checkUserInterrupt();

    std::vector<bool> destroyed(d);
    std::fill(destroyed.begin(), destroyed.end(), false);

    MatrixRow<REALSXP> values = out(k, _);
    while (!std::all_of(destroyed.begin(), destroyed.end(), [](bool v) { return v; })) {
      auto waiting_time = (*exp_generator)();
      auto affected = (*count_generator)();

      for (int i=0; i<d; i++) {
        if (!destroyed[i]) {
          values[i] += waiting_time;
          if (mo::math::is_within(i, affected)) {
            destroyed[i] = true;
          }
        }
      }
    }
  }

  return out;
}

//' @keywords internal
//' @noRd
// [[Rcpp::export]]
NumericMatrix Rcpp__rmo_ex_arnold(
    const R_xlen_t n, const int d,
    const NumericVector& ex_intensities) {
  std::vector<std::unique_ptr<ExpGenerator<RRNGPolicy>>> exp_generators(d);
  std::vector<std::unique_ptr<CountReplaceGenerator<RRNGPolicy>>> count_generators(d);
  std::unique_ptr<UnifPermutationGenerator<std::vector<R_xlen_t>, RRNGPolicy>> permutation_generator{new UnifPermutationGenerator<std::vector<R_xlen_t>, RRNGPolicy>(d)};
  for (int i=0; i<d; i++) {
    std::vector<double> intensities(d-i);
    for (int j=0; j<d-i; j++) {
      for (int k=0; k<i+1; k++) {
        intensities[j] += R::choose(i, k) * ex_intensities[k+j];
      }
      intensities[j] *= R::choose(d-i, j+1);
    }
    auto total_intensity = 0.;
    for (const auto& intensity : intensities)
      total_intensity += intensity;
    exp_generators[i].reset(new ExpGenerator<RRNGPolicy>(total_intensity));
    count_generators[i].reset(new CountReplaceGenerator<RRNGPolicy>(intensities));
  }

  NumericMatrix out(n, d);
  for (R_xlen_t k=0; k<n; k++) {
    if ((d*k) % C_CHECK_USR_INTERRUP == 0)
      checkUserInterrupt();

    std::vector<double> values(d);
    auto state = 0;
    while (state < d) {
      auto waiting_time = (*exp_generators[state])();
      for (int i=state; i<d; i++) {
        values[i] += waiting_time;
      }
      state += 1 + (*count_generators[state])();
    }
    // auto perm = sample(d, d, false, R_NilValue, false); // Use `RNGkind(sample.kind="Rounding")` for comparison, since R.3.6.x not implemented in Rcpp
    // auto perm = rpermutation(d);
    auto perm = (*permutation_generator)();
    /***
    TODO: It seems that using Rcpp::sample is slightly faster. Can this be a
    consequence of the low dimension of the test-cases? Is seems that this is overhead from using std::vector.
    the generator class?
    */

    for (int i=0; i<d; i++)
      out(k, i) = values[perm[i]];
  }
  return out;
}



//' @keywords internal
//' @noRd
// [[Rcpp::export]]
NumericMatrix Rcpp__rmo_esm_cuadras_auge(
    const R_xlen_t n, const int d,
    const double alpha, const double beta) { // alpha, beta >= 0
  if (alpha < 0. || beta < 0.)
    std::range_error("alpha or beta < 0"); // # nocov

  std::unique_ptr<ExpGenerator<RRNGPolicy>> exp_generator{new ExpGenerator<RRNGPolicy>()};

  NumericMatrix out(no_init(n, d));
  for (R_xlen_t k=0; k<n; k++) {
  // dont't use out.nrow() for performance
    if ((d*k) % C_CHECK_USR_INTERRUP == 0)
      checkUserInterrupt();

    auto global_shock = (*exp_generator)(beta);
    MatrixRow<REALSXP> values = out(k, _);
    for (auto& value : values) {
      auto individual_shock = (*exp_generator)(alpha);
      value = mo::math::min(individual_shock, global_shock);
    }
  }

  return out;
}

std::unique_ptr<RealUnivariateGenerator<double, RRNGPolicy>> get_univariate_generator(
    const std::string name, const List& args);


//' @keywords internal
//' @noRd
// [[Rcpp::export]]
NumericMatrix Rcpp__rmo_lfm_cpp(
    const R_xlen_t n, const R_xlen_t d,
    const double rate, const double rate_killing, const double rate_drift,
    const std::string rjump_name, const List& rjump_arg_list) {
  ExpGenerator<RRNGPolicy> bv_generator(1.);
  ExpGenerator<RRNGPolicy> kt_generator(rate_killing);
  ExpGenerator<RRNGPolicy> wt_generator(rate);
  std::unique_ptr<RealUnivariateGenerator<double, RRNGPolicy>> jump_generator = get_univariate_generator(rjump_name, rjump_arg_list);
  std::vector<double> barriers(d);

  NumericMatrix out(n, d);
  for (R_xlen_t k=0; k<n; k++) {
    std::generate(barriers.begin(), barriers.end(),
      bv_generator);
    auto killing_time = kt_generator();

    auto time = 0.;
    auto value = 0.;
    auto idx = mo::utils::sort_index(barriers);
    for (R_xlen_t i=0; i<d; i++) {
      while (
          i<d &&
          value < barriers[idx[i]]) {
        auto tt_jump = wt_generator();
        auto tt_killing = killing_time - time;
        auto value_jump = (*jump_generator)();

        if (tt_killing < R_PosInf && tt_killing <= tt_jump) {
          while (i<d && rate_drift > 0. &&
              (barriers[idx[i]] - value)/rate_drift <= tt_killing) {
            auto tt_drift = (barriers[idx[i]] - value) / rate_drift;
            time += tt_drift;
            value = barriers[idx[i]];
            tt_killing -= tt_drift;
            out(k, idx[i++]) = time;
          }
          time = killing_time;
          value = R_PosInf;
          while (i<d) out(k, idx[i++]) = time;
        } else {
          while (i<d && rate_drift > 0. &&
              (barriers[idx[i]] - value)/rate_drift <= tt_jump) {
            auto tt_drift = (barriers[idx[i]] - value)/rate_drift;
            time += tt_drift;
            value = barriers[idx[i]];
            tt_jump -= tt_drift;
            out(k, idx[i++]) = time;
          }
          if (tt_jump < R_PosInf) {
            time += tt_jump;
            value += tt_jump * rate_drift + value_jump;
          }
          while (i<d && value >= barriers[idx[i]]) out(k, idx[i++]) = time;
        }
      }
    }
  }
  return out;
}


//' @keywords internal
//' @noRd
std::unique_ptr<RealUnivariateGenerator<double, RRNGPolicy>> get_univariate_generator(
    const std::string name, const List& args) {
  std::unique_ptr<RealUnivariateGenerator<double, RRNGPolicy>> out;
  if ("rexp" == name) {
    double rate = args["rate"];
    out.reset(new ExpGenerator<RRNGPolicy>(rate));
  } else if ("rposval" == name) {
    double value = args["value"];
    out.reset(new FixedDblGenerator<RRNGPolicy>(value));
  } else {
    std::logic_error("wrong input"); // # nocov
  }

  return out;
}
