// clang-format off
#include <Rcpp.h>
#include <r_engine.hpp> // must be included before <rmolib/*>
// clang-format on

#include "rcpp_distribution_caller.h"
#include <rmolib/distribution.hpp>

using namespace Rcpp;

// [[Rcpp::export]]
bool Rcpp__is_within(const std::size_t i, const std::size_t j) {
    return rmolib::random::internal::is_within(i-1, j);
}

// [[Rcpp::export]]
NumericMatrix Rcpp__rmo_esm(const std::size_t n, const std::size_t d,
                            const NumericVector& intensities) {
  using exponential_distribution =
      rmolib::random::exponential_distribution<double>;
  using esm_mo_distribution =
      rmolib::random::esm_mo_distribution<double, exponential_distribution>;
  using caller_t = rcpp_distribution_caller<esm_mo_distribution>;

  return caller_t::call(r_engine{}, n, static_cast<std::size_t>(d),
                        intensities.begin(), intensities.end());
}

// [[Rcpp::export]]
NumericMatrix Rcpp__rmo_arnold(const std::size_t n, const std::size_t d,
                               const NumericVector& intensities) {
  using exponential_distribution =
      rmolib::random::exponential_distribution<double>;
  using uniform_real_distribution =
      rmolib::random::uniform_real_distribution<double>;
  using discrete_distribution =
      rmolib::random::r_discrete_distribution<std::size_t, double,
                                              uniform_real_distribution>;
  using arnold_mo_distribution =
      rmolib::random::arnold_mo_distribution<double, exponential_distribution,
                                             discrete_distribution>;
  using caller_t = rcpp_distribution_caller<arnold_mo_distribution>;

  return caller_t::call(r_engine{}, n, d, intensities.begin(),
                        intensities.end());
}

// [[Rcpp::export]]
NumericMatrix Rcpp__rmo_ex_arnold(const std::size_t n, const std::size_t d,
                                  const NumericVector& ex_intensities) {
  // R's sample.int produces a final (redundant) selection of the
  // last remaining value see
  // https://github.com/wch/r-source/blob/613bdfd0e1d3fc9984142d5da3da448adf2438c7/src/main/random.c#L461
  using exponential_distribution =
      rmolib::random::exponential_distribution<double>;
  using uniform_real_distribution =
      rmolib::random::uniform_real_distribution<double>;
  using uniform_int_distribution =
      rmolib::random::uniform_int_distribution<std::size_t>;
  using discrete_distribution =
      rmolib::random::r_discrete_distribution<std::size_t, double,
                                              uniform_real_distribution>;
  using markovian_exmo_distribution =
      rmolib::random::markovian_exmo_distribution<
          double, exponential_distribution, uniform_int_distribution,
          discrete_distribution>;
  using caller_t = rcpp_distribution_caller<markovian_exmo_distribution, true>;

  return caller_t::call(r_engine{}, n, d, ex_intensities.begin(),
                        ex_intensities.end());
}

// [[Rcpp::export]]
NumericMatrix Rcpp__rmo_esm_cuadras_auge(const std::size_t n,
                                         const std::size_t d,
                                         const double alpha,
                                         const double beta) {
  using exponential_distribution =
      rmolib::random::exponential_distribution<double>;
  using cuadras_auge_distribution =
      rmolib::random::cuadras_auge_distribution<double,
                                                exponential_distribution>;
  using caller_t = rcpp_distribution_caller<cuadras_auge_distribution>;

  return caller_t::call(r_engine{}, n, d, alpha, beta);
}

// [[Rcpp::export]]
NumericMatrix Rcpp__rmo_lfm_cpp(const std::size_t n, const std::size_t d,
                                const double rate, const double rate_killing,
                                const double rate_drift,
                                const std::string rjump_name,
                                const List& rjump_arg_list) {
  using deterministic_distribution =
      rmolib::random::deterministic_distribution<double>;
  using exponential_distribution =
      rmolib::random::exponential_distribution<double>;
  using pareto_distribution = rmolib::random::pareto_distribution<double>;

  if ("rposval" == rjump_name) {
    using caller_t = rcpp_distribution_caller<rmolib::random::lfm_distribution<
        double, deterministic_distribution, exponential_distribution>>;
    return caller_t::call(r_engine{}, n, d, rate_killing, rate_drift, rate,
                          static_cast<double>(rjump_arg_list["value"]));
  } else if ("rexp" == rjump_name) {
    using caller_t = rcpp_distribution_caller<rmolib::random::lfm_distribution<
        double, exponential_distribution, exponential_distribution>>;
    return caller_t::call(r_engine{}, n, d, rate_killing, rate_drift, rate,
                          static_cast<double>(rjump_arg_list["rate"]));
  } else if ("rpareto" == rjump_name) {
    using caller_t = rcpp_distribution_caller<rmolib::random::lfm_distribution<
        double, pareto_distribution, exponential_distribution>>;
    return caller_t::call(r_engine{}, n, d, rate_killing, rate_drift, rate,
                          static_cast<double>(rjump_arg_list["alpha"]),
                          static_cast<double>(rjump_arg_list["x0"]));
  } else {
    throw std::domain_error("rjump_name not supported");
  }
}

// [[Rcpp::export]]
NumericVector mo_internal__deterministic(const std::size_t n,
                                         const double value) {
  using deterministic_distribution =
      rmolib::random::deterministic_distribution<double>;
  using caller_t = rcpp_distribution_caller<deterministic_distribution>;

  return caller_t::call(r_engine{}, n, value);
}

// [[Rcpp::export]]
NumericVector mo_internal__exponential(const std::size_t n, const double rate) {
  using exponential_distribution =
      rmolib::random::exponential_distribution<double>;
  using caller_t = rcpp_distribution_caller<exponential_distribution>;

  return caller_t::call(r_engine{}, n, rate);
}

// [[Rcpp::export]]
NumericVector mo_internal__pareto(const std::size_t n, const double alpha,
                                  const double x0) {
  using pareto_distribution = rmolib::random::pareto_distribution<double>;
  using caller_t = rcpp_distribution_caller<pareto_distribution>;

  return caller_t::call(r_engine{}, n, alpha, x0);
}

// [[Rcpp::export]]
IntegerVector mo_internal__rdiscrete(
    const std::size_t n, const std::size_t d,
    const Nullable<NumericVector> probabilities = R_NilValue) {
  if (probabilities.isNotNull()) {
    using uniform_real_distribution =
        rmolib::random::uniform_real_distribution<double>;
    using discrete_distribution =
        rmolib::random::r_discrete_distribution<std::size_t, double,
                                                uniform_real_distribution>;
    using caller_t = rcpp_distribution_caller<discrete_distribution>;
    NumericVector p(probabilities);
    return caller_t::call(r_engine{}, n, p.begin(), p.end());
  } else {
    using uniform_int_distribution =
        rmolib::random::uniform_int_distribution<std::size_t>;
    using caller_t = rcpp_distribution_caller<uniform_int_distribution>;

    return caller_t::call(r_engine{}, n, std::size_t{0}, d);
  }
}
