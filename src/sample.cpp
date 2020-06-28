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
  std::unique_ptr<PermutationGenerator<std::vector<R_xlen_t>, RRNGPolicy>> permutation_generator{new PermutationGenerator<std::vector<R_xlen_t>, RRNGPolicy>(d)};
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

std::vector<std::pair<double, double>> sample_cpp(
    const double rate, const double rate_killing, const double rate_drift,
    const std::string rjump_name, const List& rjump_arg_list,
    const std::vector<double>& barrier_values);


//' @keywords internal
//' @noRd
// [[Rcpp::export]]
NumericMatrix Rcpp__rmo_lfm_cpp(
    const R_xlen_t n, const R_xlen_t d,
    const double rate, const double rate_killing, const double rate_drift,
    const std::string rjump_name, const List& rjump_arg_list) {
  std::unique_ptr<ExpGenerator<RRNGPolicy>> bv_generator{new ExpGenerator<RRNGPolicy>(1.)};
  std::vector<double> unit_exponentials(d);
  std::vector<std::pair<double, double>> cpp_subordinator;
  int count;

  NumericMatrix out(n, d);
  for (R_xlen_t k=0; k<n; k++) {
    std::generate(unit_exponentials.begin(), unit_exponentials.end(),
      *bv_generator);
    cpp_subordinator = sample_cpp(rate, rate_killing, rate_drift, rjump_name, rjump_arg_list, unit_exponentials);
    for (int i=0; i<d; i++) {
      count = 0;
      while (cpp_subordinator[count].second < unit_exponentials[i] && count < cpp_subordinator.size())
        count += 1;

      out(k, i) = cpp_subordinator[count].first;
    }
  }
  return out;
}


std::vector<std::pair<double, double>> sample_cpp(
      const double rate, const double rate_killing, const double rate_drift,
      const std::string rjump_name, const List& rjump_arg_list,
      const std::vector<double>& barrier_values) {
  std::vector<double> barrier_values_;
  if (rate_drift>0.) {
    barrier_values_ = std::vector<double>(barrier_values);
    std::sort(barrier_values_.begin(), barrier_values_.end());
  } else {
    barrier_values_ = std::vector<double>(1, *std::max_element(barrier_values.begin(), barrier_values.end()));
  }
  std::unique_ptr<ExpGenerator<RRNGPolicy>> wt_generator{new ExpGenerator<RRNGPolicy>(rate)};
  std::unique_ptr<ExpGenerator<RRNGPolicy>> kt_generator{new ExpGenerator<RRNGPolicy>(rate_killing)};
  std::unique_ptr<RealUnivariateGenerator<double, RRNGPolicy>> jump_generator = get_univariate_generator(rjump_name, rjump_arg_list);
  R_xlen_t d = barrier_values_.size();

  double killing_time = (*kt_generator)();

  double time = 0.;
  double value = 0.;
  std::vector<std::pair<double, double>> out(1, {time, value});
  /**
   * Explanation:
   *  - the expected maximum value of d iid unit exponentials
   *    is O(d)
   *  - 5 is chosen as a conservative factor but should ultimately
   *    be replaced by a more meaningful metric (e.g. mean of the
   *    jump distribution)
   * replace d*5. by an estimate
   * of the
   */
  int estimated_length = 0;
  if (rate > 0.)
    estimated_length += std::ceil(1. + std::log(1. - std::pow(1. - 0.05, 1./d)) / std::log(1 - (*jump_generator).laplace(1.)));
  if (rate_drift > 0.)
  estimated_length += d;
  out.reserve(estimated_length);
  for (int i=0; i<d; i++) {
    while (value < barrier_values_[i]) {
      double waiting_time = (*wt_generator)();
      double killing_waiting_time = killing_time - time;
      double jump_value = (*jump_generator)();

      if (killing_waiting_time < R_PosInf && killing_waiting_time <= waiting_time) {
        for (int j=i; j<d; j++) {
          if (rate_drift > 0. && (barrier_values_[j] - value)/rate_drift <= killing_waiting_time) {
            double intermediate_waiting_time = (barrier_values_[j] - value) / rate_drift;
            time += intermediate_waiting_time;
            value = barrier_values_[j];
            out.push_back({time, value});
            killing_waiting_time -= intermediate_waiting_time;
          }
        }

        time = killing_time;
        value = R_PosInf;
        out.push_back({time, value});
      } else {
        for (int j=i; j<d; j++) {
          if (rate_drift > 0. && (barrier_values_[j] - value)/rate_drift <= waiting_time) {
            double intermediate_waiting_time = (barrier_values_[j] - value)/rate_drift;
            time += intermediate_waiting_time;
            value = barrier_values_[j];
            out.push_back({time, value});
            waiting_time -= intermediate_waiting_time;
          }
        }

        if (rate > 0.) { // waiting_time < R_PosInf
          time += waiting_time;
          value += waiting_time * rate_drift + jump_value;
          out.push_back({time, value});
        }
      }
    }
  }

  return out;
}

//' @keywords internal
//' @noRd
// [[Rcpp::export]]
NumericMatrix sample_cpp(
      const double rate, const double rate_killing, const double rate_drift,
      const std::string rjump_name, const List& rjump_arg_list,
      const NumericVector& barrier_values) {
  std::vector<std::pair<double, double>> cpp_subordinator = sample_cpp(rate, rate_killing, rate_drift, rjump_name, rjump_arg_list, Rcpp::as<std::vector<double>>(barrier_values));
  NumericMatrix out(cpp_subordinator.size(), 2);
  for (R_xlen_t i=0; i<cpp_subordinator.size(); i++) {
    out(i, 0) = cpp_subordinator[i].first;
    out(i, 1) = cpp_subordinator[i].second;
  }
  colnames(out) = CharacterVector::create("t", "value");

  return out;
}

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
