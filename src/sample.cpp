#include <algorithm>
#include <cstddef>
#include <string>

// clang-format off
#include <Rcpp.h>
#include "rmolib/random/r_engine.hpp" // must be included before <rmolib/*>
// clang-format on

#include "rcpp_distribution_caller.h"
#include "rmolib/algorithm/r_shuffle.hpp"
#include "rmolib/random/multivariate/am_mo_distribution.hpp"
#include "rmolib/random/multivariate/esm_armextmo_distribution.hpp"
#include "rmolib/random/multivariate/esm_mo_distribution.hpp"
#include "rmolib/random/multivariate/lfm_extmo_distribution.hpp"
#include "rmolib/random/multivariate/mdcm_exmo_distribution.hpp"
#include "rmolib/random/univariate/deterministic_distribution.hpp"
#include "rmolib/random/univariate/exponential_distribution.hpp"
#include "rmolib/random/univariate/pareto_distribution.hpp"
#include "rmolib/random/univariate/discrete_distribution.hpp"
#include "rmolib/random/univariate/r_discrete_distribution.hpp"
#include "rmolib/random/univariate/uniform_int_distribution.hpp"
#include "rmolib/random/univariate/uniform_real_distribution.hpp"

using namespace Rcpp;

// [[Rcpp::export]]
bool Rcpp__is_within(const std::size_t i, const std::size_t j) {
  return rmolib::random::internal::is_within(i - 1, j);
}

template <typename _T>
_T get_param(const List& parms, const std::string name) {
  const auto names = as<std::vector<std::string>>(parms.names());
  auto it = std::find(names.cbegin(), names.cend(), name);
  if (it == names.end()) throw std::domain_error(name + " not provided");
  return parms[*it];
}

// # nocov start
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
NumericMatrix Rcpp__rmo_am(const std::size_t n, const std::size_t d,
                           const NumericVector& intensities) {
  using exponential_distribution =
      rmolib::random::exponential_distribution<double>;
  using uniform_real_distribution =
      rmolib::random::uniform_real_distribution<double>;
  using uniform_int_distribution =
      rmolib::random::uniform_int_distribution<std::size_t>;
  using discrete_distribution = rmolib::random::discrete_distribution<
      std::size_t, double, uniform_real_distribution, uniform_int_distribution>;
  using am_mo_distribution =
      rmolib::random::am_mo_distribution<double, exponential_distribution,
                                         discrete_distribution>;
  using caller_t = rcpp_distribution_caller<am_mo_distribution>;

  return caller_t::call(r_engine{}, n, d, intensities.begin(),
                        intensities.end());
}

// [[Rcpp::export]]
NumericMatrix Rcpp__rexmo_mdcm(const std::size_t n, const std::size_t d,
                               const NumericVector& ex_intensities) {
  using exponential_distribution =
      rmolib::random::exponential_distribution<double>;
  using uniform_real_distribution =
      rmolib::random::uniform_real_distribution<double>;
  using uniform_int_distribution =
      rmolib::random::uniform_int_distribution<std::size_t>;
  using discrete_distribution = rmolib::random::discrete_distribution<
      std::size_t, double, uniform_real_distribution, uniform_int_distribution>;
  using mdcm_exmo_distribution = rmolib::random::mdcm_exmo_distribution<
      double, exponential_distribution, uniform_int_distribution,
      discrete_distribution, rmolib::algorithm::shuffler>;
  using caller_t = rcpp_distribution_caller<mdcm_exmo_distribution, false>;

  return caller_t::call(r_engine{}, n, d, ex_intensities.begin(),
                        ex_intensities.end());
}

// [[Rcpp::export]]
NumericMatrix Rcpp__rarmextmo_esm(const std::size_t n, const std::size_t d,
                                  const double alpha, const double beta) {
  using exponential_distribution =
      rmolib::random::exponential_distribution<double>;
  using esm_armextmo_distribution =
      rmolib::random::esm_armextmo_distribution<double,
                                                exponential_distribution>;
  using caller_t = rcpp_distribution_caller<esm_armextmo_distribution>;

  return caller_t::call(r_engine{}, n, d, alpha, beta);
}

// [[Rcpp::export]]
NumericMatrix Rcpp__rextmo_lfm(const std::size_t n, const std::size_t d,
                               const double rate, const double rate_killing,
                               const double rate_drift,
                               const std::string rjump_name,
                               const List& rjump_arg_list) {
  using deterministic_distribution =
      rmolib::random::deterministic_distribution<double>;
  using exponential_distribution =
      rmolib::random::exponential_distribution<double>;
  using uniform_real_distribution =
      rmolib::random::uniform_real_distribution<double>;
  using pareto_distribution =
      rmolib::random::pareto_distribution<double, uniform_real_distribution>;

  if ("rposval" == rjump_name) {
    using caller_t =
        rcpp_distribution_caller<rmolib::random::lfm_extmo_distribution<
            double, deterministic_distribution, exponential_distribution>>;

    const auto value = get_param<double>(rjump_arg_list, "value");
    return caller_t::call(r_engine{}, n, d, rate_killing, rate_drift, rate,
                          value);
  } else if ("rexp" == rjump_name) {
    using caller_t =
        rcpp_distribution_caller<rmolib::random::lfm_extmo_distribution<
            double, exponential_distribution, exponential_distribution>>;

    const auto lambda = get_param<double>(rjump_arg_list, "rate");
    return caller_t::call(r_engine{}, n, d, rate_killing, rate_drift, rate,
                          lambda);
  } else if ("rpareto" == rjump_name) {
    using caller_t =
        rcpp_distribution_caller<rmolib::random::lfm_extmo_distribution<
            double, pareto_distribution, exponential_distribution>>;

    const auto alpha = get_param<double>(rjump_arg_list, "alpha");
    const auto lower_bound = get_param<double>(rjump_arg_list, "x0");
    return caller_t::call(r_engine{}, n, d, rate_killing, rate_drift, rate,
                          alpha, lower_bound);
  } else {
    throw std::domain_error("rjump_name not supported");
  }
}

// # nocov end

// ----------------------------------------------------------------------------
// internal multivariate generators, exposed for testing
// ----------------------------------------------------------------------------

//' @keywords internal test
// [[Rcpp::export]]
NumericMatrix rtest__rmo_esm(const std::size_t n, const std::size_t d,
                             const NumericVector& intensities) {
  return Rcpp__rmo_esm(n, d, intensities);
}

//' @keywords internal test
// [[Rcpp::export]]
NumericMatrix rtest__rmo_am(const std::size_t n, const std::size_t d,
                            const NumericVector& intensities) {
  using exponential_distribution =
      rmolib::random::exponential_distribution<double>;
  using uniform_real_distribution =
      rmolib::random::uniform_real_distribution<double>;
  using discrete_distribution =
      rmolib::random::r_discrete_distribution<std::size_t, double,
                                              uniform_real_distribution>;
  using am_mo_distribution =
      rmolib::random::am_mo_distribution<double, exponential_distribution,
                                         discrete_distribution>;
  using caller_t = rcpp_distribution_caller<am_mo_distribution>;

  return caller_t::call(r_engine{}, n, d, intensities.begin(),
                        intensities.end());
}

//' @keywords internal test
// [[Rcpp::export]]
NumericMatrix rtest__rexmo_mdcm(const std::size_t n, const std::size_t d,
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
  using mdcm_exmo_distribution = rmolib::random::mdcm_exmo_distribution<
      double, exponential_distribution, uniform_int_distribution,
      discrete_distribution, rmolib::algorithm::r_shuffler>;
  using caller_t = rcpp_distribution_caller<mdcm_exmo_distribution, true>;

  return caller_t::call(r_engine{}, n, d, ex_intensities.begin(),
                        ex_intensities.end());
}

//' @keywords internal test
// [[Rcpp::export]]
NumericMatrix rtest__rarmextmo_esm(const std::size_t n, const std::size_t d,
                                   const double alpha, const double beta) {
  return Rcpp__rarmextmo_esm(n, d, alpha, beta);
}

//' @keywords internal test
// [[Rcpp::export]]
NumericMatrix rtest__rextmo_lfm(const std::size_t n, const std::size_t d,
                                const double rate, const double rate_killing,
                                const double rate_drift,
                                const std::string rjump_name,
                                const List& rjump_arg_list) {
  return Rcpp__rextmo_lfm(n, d, rate, rate_killing, rate_drift, rjump_name,
                          rjump_arg_list);
}

// ----------------------------------------------------------------------------
// internal univariate generators, exposed for testing
// ----------------------------------------------------------------------------

//' @keywords internal test
// [[Rcpp::export]]
NumericVector rtest__deterministic(const std::size_t n, const double value) {
  using deterministic_distribution =
      rmolib::random::deterministic_distribution<double>;
  using caller_t = rcpp_distribution_caller<deterministic_distribution>;

  return caller_t::call(r_engine{}, n, value);
}

//' @keywords internal test
// [[Rcpp::export]]
NumericVector rtest__exponential(const std::size_t n, const double rate) {
  using exponential_distribution =
      rmolib::random::exponential_distribution<double>;
  using caller_t = rcpp_distribution_caller<exponential_distribution>;

  return caller_t::call(r_engine{}, n, rate);
}

//' @keywords internal test
// [[Rcpp::export]]
NumericVector rtest__pareto(const std::size_t n, const double alpha,
                            const double x0) {
  using uniform_real_distribution =
      rmolib::random::uniform_real_distribution<double>;
  using pareto_distribution =
      rmolib::random::pareto_distribution<double, uniform_real_distribution>;
  using caller_t = rcpp_distribution_caller<pareto_distribution>;

  return caller_t::call(r_engine{}, n, alpha, x0);
}

//' @keywords internal test
// [[Rcpp::export]]
IntegerVector rtest__discrete(
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
