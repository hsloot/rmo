// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// multiply_binomial_coefficient
NumericVector multiply_binomial_coefficient(const NumericVector& x, const std::size_t n, const std::size_t k);
RcppExport SEXP _rmo_multiply_binomial_coefficient(SEXP xSEXP, SEXP nSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericVector& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const std::size_t >::type n(nSEXP);
    Rcpp::traits::input_parameter< const std::size_t >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(multiply_binomial_coefficient(x, n, k));
    return rcpp_result_gen;
END_RCPP
}
// divide_binomial_coefficient
NumericVector divide_binomial_coefficient(const NumericVector& x, const std::size_t n, const std::size_t k);
RcppExport SEXP _rmo_divide_binomial_coefficient(SEXP xSEXP, SEXP nSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericVector& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const std::size_t >::type n(nSEXP);
    Rcpp::traits::input_parameter< const std::size_t >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(divide_binomial_coefficient(x, n, k));
    return rcpp_result_gen;
END_RCPP
}
// uexi2i
NumericVector uexi2i(const NumericVector& uexi);
RcppExport SEXP _rmo_uexi2i(SEXP uexiSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const NumericVector& >::type uexi(uexiSEXP);
    rcpp_result_gen = Rcpp::wrap(uexi2i(uexi));
    return rcpp_result_gen;
END_RCPP
}
// exi2exqm
NumericMatrix exi2exqm(const NumericVector& exi);
RcppExport SEXP _rmo_exi2exqm(SEXP exiSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const NumericVector& >::type exi(exiSEXP);
    rcpp_result_gen = Rcpp::wrap(exi2exqm(exi));
    return rcpp_result_gen;
END_RCPP
}
// Rcpp__is_within
bool Rcpp__is_within(const std::size_t i, const std::size_t j);
RcppExport SEXP _rmo_Rcpp__is_within(SEXP iSEXP, SEXP jSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::size_t >::type i(iSEXP);
    Rcpp::traits::input_parameter< const std::size_t >::type j(jSEXP);
    rcpp_result_gen = Rcpp::wrap(Rcpp__is_within(i, j));
    return rcpp_result_gen;
END_RCPP
}
// Rcpp__rmo_esm
NumericMatrix Rcpp__rmo_esm(const std::size_t n, const std::size_t d, const NumericVector& intensities);
RcppExport SEXP _rmo_Rcpp__rmo_esm(SEXP nSEXP, SEXP dSEXP, SEXP intensitiesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::size_t >::type n(nSEXP);
    Rcpp::traits::input_parameter< const std::size_t >::type d(dSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type intensities(intensitiesSEXP);
    rcpp_result_gen = Rcpp::wrap(Rcpp__rmo_esm(n, d, intensities));
    return rcpp_result_gen;
END_RCPP
}
// Rcpp__rmo_arnold
NumericMatrix Rcpp__rmo_arnold(const std::size_t n, const std::size_t d, const NumericVector& intensities);
RcppExport SEXP _rmo_Rcpp__rmo_arnold(SEXP nSEXP, SEXP dSEXP, SEXP intensitiesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::size_t >::type n(nSEXP);
    Rcpp::traits::input_parameter< const std::size_t >::type d(dSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type intensities(intensitiesSEXP);
    rcpp_result_gen = Rcpp::wrap(Rcpp__rmo_arnold(n, d, intensities));
    return rcpp_result_gen;
END_RCPP
}
// Rcpp__rexmo_markovian
NumericMatrix Rcpp__rexmo_markovian(const std::size_t n, const std::size_t d, const NumericVector& ex_intensities);
RcppExport SEXP _rmo_Rcpp__rexmo_markovian(SEXP nSEXP, SEXP dSEXP, SEXP ex_intensitiesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::size_t >::type n(nSEXP);
    Rcpp::traits::input_parameter< const std::size_t >::type d(dSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type ex_intensities(ex_intensitiesSEXP);
    rcpp_result_gen = Rcpp::wrap(Rcpp__rexmo_markovian(n, d, ex_intensities));
    return rcpp_result_gen;
END_RCPP
}
// Rcpp__rarmextmo_esm
NumericMatrix Rcpp__rarmextmo_esm(const std::size_t n, const std::size_t d, const double alpha, const double beta);
RcppExport SEXP _rmo_Rcpp__rarmextmo_esm(SEXP nSEXP, SEXP dSEXP, SEXP alphaSEXP, SEXP betaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::size_t >::type n(nSEXP);
    Rcpp::traits::input_parameter< const std::size_t >::type d(dSEXP);
    Rcpp::traits::input_parameter< const double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< const double >::type beta(betaSEXP);
    rcpp_result_gen = Rcpp::wrap(Rcpp__rarmextmo_esm(n, d, alpha, beta));
    return rcpp_result_gen;
END_RCPP
}
// Rcpp__rextmo_lfm
NumericMatrix Rcpp__rextmo_lfm(const std::size_t n, const std::size_t d, const double rate, const double rate_killing, const double rate_drift, const std::string rjump_name, const List& rjump_arg_list);
RcppExport SEXP _rmo_Rcpp__rextmo_lfm(SEXP nSEXP, SEXP dSEXP, SEXP rateSEXP, SEXP rate_killingSEXP, SEXP rate_driftSEXP, SEXP rjump_nameSEXP, SEXP rjump_arg_listSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::size_t >::type n(nSEXP);
    Rcpp::traits::input_parameter< const std::size_t >::type d(dSEXP);
    Rcpp::traits::input_parameter< const double >::type rate(rateSEXP);
    Rcpp::traits::input_parameter< const double >::type rate_killing(rate_killingSEXP);
    Rcpp::traits::input_parameter< const double >::type rate_drift(rate_driftSEXP);
    Rcpp::traits::input_parameter< const std::string >::type rjump_name(rjump_nameSEXP);
    Rcpp::traits::input_parameter< const List& >::type rjump_arg_list(rjump_arg_listSEXP);
    rcpp_result_gen = Rcpp::wrap(Rcpp__rextmo_lfm(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list));
    return rcpp_result_gen;
END_RCPP
}
// rtest__rmo_esm
NumericMatrix rtest__rmo_esm(const std::size_t n, const std::size_t d, const NumericVector& intensities);
RcppExport SEXP _rmo_rtest__rmo_esm(SEXP nSEXP, SEXP dSEXP, SEXP intensitiesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::size_t >::type n(nSEXP);
    Rcpp::traits::input_parameter< const std::size_t >::type d(dSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type intensities(intensitiesSEXP);
    rcpp_result_gen = Rcpp::wrap(rtest__rmo_esm(n, d, intensities));
    return rcpp_result_gen;
END_RCPP
}
// rtest__rmo_arnold
NumericMatrix rtest__rmo_arnold(const std::size_t n, const std::size_t d, const NumericVector& intensities);
RcppExport SEXP _rmo_rtest__rmo_arnold(SEXP nSEXP, SEXP dSEXP, SEXP intensitiesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::size_t >::type n(nSEXP);
    Rcpp::traits::input_parameter< const std::size_t >::type d(dSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type intensities(intensitiesSEXP);
    rcpp_result_gen = Rcpp::wrap(rtest__rmo_arnold(n, d, intensities));
    return rcpp_result_gen;
END_RCPP
}
// rtest__rexmo_markovian
NumericMatrix rtest__rexmo_markovian(const std::size_t n, const std::size_t d, const NumericVector& ex_intensities);
RcppExport SEXP _rmo_rtest__rexmo_markovian(SEXP nSEXP, SEXP dSEXP, SEXP ex_intensitiesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::size_t >::type n(nSEXP);
    Rcpp::traits::input_parameter< const std::size_t >::type d(dSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type ex_intensities(ex_intensitiesSEXP);
    rcpp_result_gen = Rcpp::wrap(rtest__rexmo_markovian(n, d, ex_intensities));
    return rcpp_result_gen;
END_RCPP
}
// rtest__rarmextmo_esm
NumericMatrix rtest__rarmextmo_esm(const std::size_t n, const std::size_t d, const double alpha, const double beta);
RcppExport SEXP _rmo_rtest__rarmextmo_esm(SEXP nSEXP, SEXP dSEXP, SEXP alphaSEXP, SEXP betaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::size_t >::type n(nSEXP);
    Rcpp::traits::input_parameter< const std::size_t >::type d(dSEXP);
    Rcpp::traits::input_parameter< const double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< const double >::type beta(betaSEXP);
    rcpp_result_gen = Rcpp::wrap(rtest__rarmextmo_esm(n, d, alpha, beta));
    return rcpp_result_gen;
END_RCPP
}
// rtest__rextmo_lfm
NumericMatrix rtest__rextmo_lfm(const std::size_t n, const std::size_t d, const double rate, const double rate_killing, const double rate_drift, const std::string rjump_name, const List& rjump_arg_list);
RcppExport SEXP _rmo_rtest__rextmo_lfm(SEXP nSEXP, SEXP dSEXP, SEXP rateSEXP, SEXP rate_killingSEXP, SEXP rate_driftSEXP, SEXP rjump_nameSEXP, SEXP rjump_arg_listSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::size_t >::type n(nSEXP);
    Rcpp::traits::input_parameter< const std::size_t >::type d(dSEXP);
    Rcpp::traits::input_parameter< const double >::type rate(rateSEXP);
    Rcpp::traits::input_parameter< const double >::type rate_killing(rate_killingSEXP);
    Rcpp::traits::input_parameter< const double >::type rate_drift(rate_driftSEXP);
    Rcpp::traits::input_parameter< const std::string >::type rjump_name(rjump_nameSEXP);
    Rcpp::traits::input_parameter< const List& >::type rjump_arg_list(rjump_arg_listSEXP);
    rcpp_result_gen = Rcpp::wrap(rtest__rextmo_lfm(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list));
    return rcpp_result_gen;
END_RCPP
}
// rtest__deterministic
NumericVector rtest__deterministic(const std::size_t n, const double value);
RcppExport SEXP _rmo_rtest__deterministic(SEXP nSEXP, SEXP valueSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::size_t >::type n(nSEXP);
    Rcpp::traits::input_parameter< const double >::type value(valueSEXP);
    rcpp_result_gen = Rcpp::wrap(rtest__deterministic(n, value));
    return rcpp_result_gen;
END_RCPP
}
// rtest__exponential
NumericVector rtest__exponential(const std::size_t n, const double rate);
RcppExport SEXP _rmo_rtest__exponential(SEXP nSEXP, SEXP rateSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::size_t >::type n(nSEXP);
    Rcpp::traits::input_parameter< const double >::type rate(rateSEXP);
    rcpp_result_gen = Rcpp::wrap(rtest__exponential(n, rate));
    return rcpp_result_gen;
END_RCPP
}
// rtest__pareto
NumericVector rtest__pareto(const std::size_t n, const double alpha, const double x0);
RcppExport SEXP _rmo_rtest__pareto(SEXP nSEXP, SEXP alphaSEXP, SEXP x0SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::size_t >::type n(nSEXP);
    Rcpp::traits::input_parameter< const double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< const double >::type x0(x0SEXP);
    rcpp_result_gen = Rcpp::wrap(rtest__pareto(n, alpha, x0));
    return rcpp_result_gen;
END_RCPP
}
// rtest__discrete
IntegerVector rtest__discrete(const std::size_t n, const std::size_t d, const Nullable<NumericVector> probabilities);
RcppExport SEXP _rmo_rtest__discrete(SEXP nSEXP, SEXP dSEXP, SEXP probabilitiesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::size_t >::type n(nSEXP);
    Rcpp::traits::input_parameter< const std::size_t >::type d(dSEXP);
    Rcpp::traits::input_parameter< const Nullable<NumericVector> >::type probabilities(probabilitiesSEXP);
    rcpp_result_gen = Rcpp::wrap(rtest__discrete(n, d, probabilities));
    return rcpp_result_gen;
END_RCPP
}

RcppExport SEXP run_testthat_tests(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_rmo_multiply_binomial_coefficient", (DL_FUNC) &_rmo_multiply_binomial_coefficient, 3},
    {"_rmo_divide_binomial_coefficient", (DL_FUNC) &_rmo_divide_binomial_coefficient, 3},
    {"_rmo_uexi2i", (DL_FUNC) &_rmo_uexi2i, 1},
    {"_rmo_exi2exqm", (DL_FUNC) &_rmo_exi2exqm, 1},
    {"_rmo_Rcpp__is_within", (DL_FUNC) &_rmo_Rcpp__is_within, 2},
    {"_rmo_Rcpp__rmo_esm", (DL_FUNC) &_rmo_Rcpp__rmo_esm, 3},
    {"_rmo_Rcpp__rmo_arnold", (DL_FUNC) &_rmo_Rcpp__rmo_arnold, 3},
    {"_rmo_Rcpp__rexmo_markovian", (DL_FUNC) &_rmo_Rcpp__rexmo_markovian, 3},
    {"_rmo_Rcpp__rarmextmo_esm", (DL_FUNC) &_rmo_Rcpp__rarmextmo_esm, 4},
    {"_rmo_Rcpp__rextmo_lfm", (DL_FUNC) &_rmo_Rcpp__rextmo_lfm, 7},
    {"_rmo_rtest__rmo_esm", (DL_FUNC) &_rmo_rtest__rmo_esm, 3},
    {"_rmo_rtest__rmo_arnold", (DL_FUNC) &_rmo_rtest__rmo_arnold, 3},
    {"_rmo_rtest__rexmo_markovian", (DL_FUNC) &_rmo_rtest__rexmo_markovian, 3},
    {"_rmo_rtest__rarmextmo_esm", (DL_FUNC) &_rmo_rtest__rarmextmo_esm, 4},
    {"_rmo_rtest__rextmo_lfm", (DL_FUNC) &_rmo_rtest__rextmo_lfm, 7},
    {"_rmo_rtest__deterministic", (DL_FUNC) &_rmo_rtest__deterministic, 2},
    {"_rmo_rtest__exponential", (DL_FUNC) &_rmo_rtest__exponential, 2},
    {"_rmo_rtest__pareto", (DL_FUNC) &_rmo_rtest__pareto, 3},
    {"_rmo_rtest__discrete", (DL_FUNC) &_rmo_rtest__discrete, 3},
    {"run_testthat_tests", (DL_FUNC) &run_testthat_tests, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_rmo(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
