#include <Rcpp.h>
#include <mo.hpp>
#include <math.h>
#include "sets.h"
#include "math.h"

using namespace Rcpp;

static const unsigned int C_CHECK_USR_INTERRUP = 100000;

//' @keywords internal
//' @noRd
// [[Rcpp::export]]
NumericMatrix Rcpp__rmo_esm(R_xlen_t n, R_xlen_t d, const NumericVector& intensities) {
  double shock_time;
  std::unique_ptr<mo::stats::ExpGenerator> exp_generator{new mo::stats::RExpGenerator(1.)};

  if ((1<<d)-1 != intensities.size())
    std::range_error("intensities.size() != 2^d-1");

  NumericMatrix out(no_init(n, d));
  for (R_xlen_t k=0; k<n; k++) {
    if ((d*k) % C_CHECK_USR_INTERRUP == 0)
      checkUserInterrupt();
    MatrixRow<REALSXP> values = out(k, _);
    std::fill(values.begin(), values.end(), R_PosInf);
    for (R_xlen_t j=0; j<(1<<d)-1; j++) {
    // dont't use intensities.size() for performance
      if (intensities[j] > 0.) {
        shock_time = (*exp_generator.get())(intensities[j]);
        for (R_xlen_t i=0; i<d; i++) {
        // don't use values.size() for performance
          if (mo::math::is_within(i, j)) {
              values[i] = mo::math::min(values[i], shock_time);
          }
        }
      }
    }
  }
  return out;
} // NumericMatrix Rcpp__rmo_esm(R_xlen_t n, R_xlen_t d, const NumericVector& intensities);


//' @keywords internal
//' @noRd
// [[Rcpp::export]]
NumericMatrix Rcpp__rmo_arnold(unsigned int n, unsigned int d, NumericVector intensities) {
  double total_intensity, waiting_time;
  unsigned int affected;
  LogicalVector destroyed;
  NumericVector transition_probs, values;

  total_intensity = sum(intensities);
  transition_probs = intensities / total_intensity;

  NumericMatrix out(n, d);
  for (unsigned int k=0; k<n; k++) {
    if ((d*k) % C_CHECK_USR_INTERRUP == 0)
      checkUserInterrupt();

    destroyed = LogicalVector(d, false);
    values = NumericVector(d, 0.);

    while (is_false(all(destroyed))) {
      waiting_time = R::exp_rand() / total_intensity;
      affected = sample((int) pow(2., d)-1, 1, false, transition_probs, true)[0];

      for (unsigned int i=0; i<d; i++) {
        if (!destroyed[i]) {
          values[i] += waiting_time;
          if (is_within(i+1, affected)) {
            destroyed[i] = true;
          }
        }
      }
    }

    out(k, _) = values;
  }

  return wrap( out );
} // NumericMatrix Rcpp__rmo_arnold(unsigned int n, unsigned int d, NumericVector intensities);


//' @keywords internal
//' @noRd
// [[Rcpp::export]]
NumericMatrix Rcpp__rmo_ex_arnold(unsigned int n, unsigned int d, NumericVector ex_intensities) {
  double tmp, waiting_time;
  unsigned int state;
  double total_intensity;
  NumericVector transition_probs;
  NumericVector values;
  IntegerVector perm;

  NumericMatrix generator_matrix(d+1, d+1);
  for (unsigned int i=0; i<d+1; i++) {
    for (unsigned int j=0; j<d+1; j++) {
      if (j < i) {
        generator_matrix(i, j) = 0.;
      } else if (j > i) {
        tmp = 0.;
        for (unsigned int k=0; k<i+1; k++) {
          tmp += R::choose(i, k) * ex_intensities[k+j-i-1];
        }
        tmp *= R::choose(d-i, (j-i));
        generator_matrix(i, j) = tmp;
      }
    }
    generator_matrix(i, i) = -sum(generator_matrix(i, _));
  }

  NumericMatrix out(n, d);
  for (unsigned int k=0; k<n; k++) {
    if ((d*k) % C_CHECK_USR_INTERRUP == 0)
      checkUserInterrupt();

    values = NumericVector(d, 0.);
    state = 0;
    while (state < d) {
      total_intensity = -generator_matrix(state, state);
      transition_probs = NumericVector(d-state, 0.);
      for (unsigned int i=state+1; i<d+1; i++) {
        transition_probs[i-state-1] = generator_matrix(state, i) / total_intensity;
      }
      waiting_time = R::exp_rand() / total_intensity;
      for (unsigned int i=state; i<d; i++) {
        values[i] += waiting_time;
      }
      state += 1 + sample(d-state, 1, false, transition_probs, false)[0];
    }

    perm = sample(d, d, false, R_NilValue, false); // Use `RNGkind(sample.kind="Rounding")` for comparison, since R.3.6.x not implemented in Rcpp
    values = values[perm];
    out(k, _) = values;
  }
  return wrap( out );
} // NumericMatrix Rcpp__rmo_ex_arnold(unsigned int n, unsigned int d, NumericVector ex_intensities);



//' @keywords internal
//' @noRd
// [[Rcpp::export]]
NumericMatrix Rcpp__rmo_esm_cuadras_auge(unsigned int n, unsigned int d, double alpha, double beta) { // alpha, beta >= 0
  NumericVector individual_shocks;
  double global_shock;

  NumericMatrix out(n, d);
  for (unsigned int k=0; k<n; k++) {
    if ((d*k) % C_CHECK_USR_INTERRUP == 0)
      checkUserInterrupt();

    individual_shocks = Rcpp::rexp(d, alpha);
    global_shock = ((0. == beta) ? R_PosInf : exp_rand() / beta);

    for (unsigned int i=0; i<d; i++) {
      out(k, i) = min2(individual_shocks[i], global_shock);
    }
  }

  return wrap( out );
} // NumericMatrix Rcpp__rmo_esm_cuadras_auge(unsigned int n, unsigned int d, double alpha, double beta);



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
NumericMatrix sample_cpp(double rate, double rate_killing, double rate_drift, Function rjump, List rjump_arg_list, NumericVector barrier_values) {
  barrier_values = clone(barrier_values);
  if (rate_drift>0.) {
    std::sort(barrier_values.begin(), barrier_values.end());
  } else {
    barrier_values = NumericVector(1, max(barrier_values));
  }
  unsigned int d = barrier_values.size();

  double waiting_time;
  double jump_value;
  double killing_waiting_time;

  double intermediate_waiting_time;

  Function do_call("do.call");
  rjump_arg_list.push_back(1, "n");

  std::vector<double> times(1);
  std::vector<double> values(1);
  for (unsigned int i=0; i<d; i++) {
    while (values.back() < barrier_values[i]) {
      waiting_time = ((0. == rate) ? R_PosInf : R::exp_rand()/rate);
      // requires RNGstate synchronisation
      PutRNGstate();
      jump_value = as<double>(do_call(rjump, rjump_arg_list));
      GetRNGstate();
      killing_waiting_time = ((0. == rate_killing) ? R_PosInf : R::exp_rand()/rate_killing);

      if (killing_waiting_time < R_PosInf && killing_waiting_time <= waiting_time) {
        for (unsigned int j=i; j<d; j++) {
          if (rate_drift > 0. && (barrier_values[j] - values.back())/rate_drift <= killing_waiting_time) {
            intermediate_waiting_time = (barrier_values[j] - values.back()) / rate_drift;
            times.push_back(times.back() + intermediate_waiting_time);
            values.push_back(barrier_values[j]);
            killing_waiting_time -= intermediate_waiting_time;
          }
        }

        times.push_back(times.back() + killing_waiting_time);
        values.push_back(R_PosInf);
      } else {
        for (unsigned int j=i; j<d; j++) {
          if (rate_drift > 0. && (barrier_values[j] - values.back())/rate_drift <= waiting_time) {
            intermediate_waiting_time = (barrier_values[j] - values.back())/rate_drift;
            times.push_back(times.back() + intermediate_waiting_time);
            values.push_back(barrier_values[j]);
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
  for (unsigned int i=0; i<times.size(); i++) {
    out(i, 0) = times[i];
    out(i, 1) = values[i];
  }
  colnames(out) = CharacterVector::create("t", "value");

  return out;
} // NumericMatrix sample_cpp(double rate, double rate_killing, double rate_drift, Function rjump, List rjump_arg_list, NumericVector barrier_values);


//' @keywords internal
//' @noRd
// [[Rcpp::export]]
NumericMatrix Rcpp__rmo_lfm_cpp(unsigned int n, unsigned int d, double rate, double rate_killing, double rate_drift, Function rjump, List rjump_arg_list) {
  NumericVector unit_exponentials(d);
  NumericMatrix cpp_subordinator;
  unsigned int count;

  NumericMatrix out(n, d);
  for (unsigned int k=0; k<n; k++) {
    unit_exponentials = Rcpp::rexp(d);
    cpp_subordinator = sample_cpp(rate, rate_killing, rate_drift, rjump, rjump_arg_list, unit_exponentials);
    for (unsigned int i=0; i<d; i++) {
      count = 0;
      while (cpp_subordinator(count, 1) < unit_exponentials[i] && count < (unsigned int) cpp_subordinator.nrow())
        count += 1;

      if ((unsigned int) cpp_subordinator.nrow() == count)
        stop("internal error: exponential value out of subordinator range");

      out(k, i) = cpp_subordinator(count, 0);
    }
  }
  return out;
} // NumericMatrix Rcpp__rmo_lfm_cpp(unsigned int n, unsigned int d, double rate, double rate_killing, double rate_drift, Function rjump, List rjump_arg_list);
