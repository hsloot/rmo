#include <Rcpp.h>
#include <mo.hpp>
#include <math.h>
#include "sets.h"
#include "math.h"

static const R_xlen_t C_CHECK_USR_INTERRUP = 100000;

using namespace Rcpp;
using mo::stats::RRNGPolicy;
using UnivariateGenerator = mo::stats::UnivariateGenerator<double, RRNGPolicy>;
using ExpGenerator = mo::stats::ExpGenerator<RRNGPolicy>;
using FixedDblGenerator = mo::stats::FixedDblGenerator<RRNGPolicy>;
using CountReplaceGenerator = mo::stats::CountReplaceGenerator<RRNGPolicy>;
using PermutationGenerator = mo::stats::PermutationGenerator<std::vector<R_xlen_t>, RRNGPolicy>;


//' @keywords internal
//' @noRd
// [[Rcpp::export]]
NumericMatrix Rcpp__rmo_esm(
    const R_xlen_t& n, R_xlen_t d,
    const NumericVector& intensities) {
  auto num_shocks = intensities.size();
  if ((1<<d)-1 != num_shocks)
    std::range_error("intensities.size() != 2^d-1");

  std::unique_ptr<ExpGenerator> exp_generator{new ExpGenerator()};

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
    const R_xlen_t& n, const int& d,
    const NumericVector& intensities) {
  auto total_intensity = sum(intensities);
  std::unique_ptr<ExpGenerator> exp_generator{new ExpGenerator(total_intensity)};
  std::unique_ptr<CountReplaceGenerator> count_generator{new CountReplaceGenerator(intensities)};

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
    const R_xlen_t& n, const int& d,
    const NumericVector& ex_intensities) {
  std::vector<std::unique_ptr<ExpGenerator>> exp_generators(d);
  std::vector<std::unique_ptr<CountReplaceGenerator>> count_generators(d);
  std::unique_ptr<PermutationGenerator> permutation_generator{new PermutationGenerator(d)};
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
    exp_generators[i].reset(new ExpGenerator(total_intensity));
    count_generators[i].reset(new CountReplaceGenerator(intensities));
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
    const R_xlen_t& n, const int& d,
    const double& alpha, const double& beta) { // alpha, beta >= 0
  if (alpha < 0. || beta < 0.)
    std::range_error("alpha or beta < 0");

  std::unique_ptr<ExpGenerator> exp_generator{new ExpGenerator()};

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

std::unique_ptr<UnivariateGenerator> get_univariate_generator(
    const std::string& name, const List& args);

NumericMatrix sample_cpp(
    const double& rate, const double& rate_killing, const double& rate_drift,
    const std::string& rjump_name, const List& rjump_arg_list,
    const NumericVector& barrier_values);


//' @keywords internal
//' @noRd
// [[Rcpp::export]]
NumericMatrix Rcpp__rmo_lfm_cpp(
    const R_xlen_t& n, const R_xlen_t& d,
    const double& rate, const double& rate_killing, const double& rate_drift,
    const std::string& rjump_name, const List& rjump_arg_list) {
  std::unique_ptr<ExpGenerator> bv_generator{new ExpGenerator(1.)};
  NumericVector unit_exponentials(d);
  NumericMatrix cpp_subordinator;
  int count;

  NumericMatrix out(n, d);
  for (R_xlen_t k=0; k<n; k++) {
    std::generate(unit_exponentials.begin(), unit_exponentials.end(),
      *bv_generator);
    cpp_subordinator = sample_cpp(rate, rate_killing, rate_drift, rjump_name, rjump_arg_list, unit_exponentials);
    for (int i=0; i<d; i++) {
      count = 0;
      while (cpp_subordinator(count, 1) < unit_exponentials[i] && count < cpp_subordinator.nrow())
        count += 1;

      if (cpp_subordinator.nrow() == count)
        stop("internal error: exponential value out of subordinator range");

      out(k, i) = cpp_subordinator(count, 0);
    }
  }
  return out;
}


//' @rdname rmo_lfm_cpp
//'
//' A sampling function for a (possibly killed) compound Poisson subordinator
//' with non-negative jump distribution.
//'
//' @inheritParams rmo_lfm_cpp
//' @param barrier_values a vector of barrier values from the LFM to properly
//' incorporate first exit times over these `barrier_values` if killing or drift
//' is present.
//'
//' @return A named `k x 2` array with names `c("t", "value")`, where `k` is
//' random and each row represents a time-value tupel for a jump in the compound
//' Poisson subordinator.
//'
//' @include assert.R
//' @importFrom stats rexp
//'
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
NumericMatrix sample_cpp(
      const double& rate, const double& rate_killing, const double& rate_drift,
      const std::string& rjump_name, const List& rjump_arg_list,
      const NumericVector& barrier_values) {
  NumericVector barrier_values_ = clone(barrier_values);
  if (rate_drift>0.) {
    std::sort(barrier_values_.begin(), barrier_values_.end());
  } else {
    barrier_values_ = NumericVector(1, max(barrier_values_));
  }
  std::unique_ptr<ExpGenerator> wt_generator{new ExpGenerator(rate)};
  std::unique_ptr<ExpGenerator> kt_generator{new ExpGenerator(rate_killing)};
  std::unique_ptr<UnivariateGenerator> jump_generator = get_univariate_generator(rjump_name, rjump_arg_list);
  R_xlen_t d = barrier_values_.size();

  double waiting_time;
  double jump_value;
  double killing_waiting_time;

  double intermediate_waiting_time;

  std::vector<double> times(1);
  std::vector<double> values(1);
  for (int i=0; i<d; i++) {
    while (values.back() < barrier_values_[i]) {
      waiting_time = (*wt_generator)();
      jump_value = (*jump_generator)();
      killing_waiting_time = (*kt_generator)();

      if (killing_waiting_time < R_PosInf && killing_waiting_time <= waiting_time) {
        for (int j=i; j<d; j++) {
          if (rate_drift > 0. && (barrier_values_[j] - values.back())/rate_drift <= killing_waiting_time) {
            intermediate_waiting_time = (barrier_values_[j] - values.back()) / rate_drift;
            times.push_back(times.back() + intermediate_waiting_time);
            values.push_back(barrier_values_[j]);
            killing_waiting_time -= intermediate_waiting_time;
          }
        }

        times.push_back(times.back() + killing_waiting_time);
        values.push_back(R_PosInf);
      } else {
        for (int j=i; j<d; j++) {
          if (rate_drift > 0. && (barrier_values_[j] - values.back())/rate_drift <= waiting_time) {
            intermediate_waiting_time = (barrier_values_[j] - values.back())/rate_drift;
            times.push_back(times.back() + intermediate_waiting_time);
            values.push_back(barrier_values_[j]);
            waiting_time -= intermediate_waiting_time;
          }
        }

        if (rate > 0.) { // waiting_time < R_PosInf
          times.push_back(times.back() + waiting_time);
          values.push_back(values.back() + waiting_time * rate_drift + jump_value);
        }
      }
    }
  }

  NumericMatrix out(times.size(), 2);
  for (R_xlen_t i=0; i<times.size(); i++) {
    out(i, 0) = times[i];
    out(i, 1) = values[i];
  }
  colnames(out) = CharacterVector::create("t", "value");

  return out;
}

std::unique_ptr<UnivariateGenerator> get_univariate_generator(
    const std::string& name, const List& args) {
  std::unique_ptr<UnivariateGenerator> out;
  if ("rexp" == name) {
    double rate = args["rate"];
    out.reset(new ExpGenerator(rate));
  } else if ("rposval" == name) {
    double value = args["value"];
    out.reset(new FixedDblGenerator(value));
  } else {
    std::logic_error("wrong input");
  }

  return out;
}
