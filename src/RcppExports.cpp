// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// Rcpp__rmo_esm
NumericMatrix Rcpp__rmo_esm(const R_xlen_t n, const R_xlen_t d, const NumericVector& intensities);
RcppExport SEXP _rmo_Rcpp__rmo_esm(SEXP nSEXP, SEXP dSEXP, SEXP intensitiesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const R_xlen_t >::type n(nSEXP);
    Rcpp::traits::input_parameter< const R_xlen_t >::type d(dSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type intensities(intensitiesSEXP);
    rcpp_result_gen = Rcpp::wrap(Rcpp__rmo_esm(n, d, intensities));
    return rcpp_result_gen;
END_RCPP
}
// Rcpp__rmo_arnold
NumericMatrix Rcpp__rmo_arnold(const R_xlen_t n, const int d, const NumericVector& intensities);
RcppExport SEXP _rmo_Rcpp__rmo_arnold(SEXP nSEXP, SEXP dSEXP, SEXP intensitiesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const R_xlen_t >::type n(nSEXP);
    Rcpp::traits::input_parameter< const int >::type d(dSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type intensities(intensitiesSEXP);
    rcpp_result_gen = Rcpp::wrap(Rcpp__rmo_arnold(n, d, intensities));
    return rcpp_result_gen;
END_RCPP
}
// Rcpp__rmo_ex_arnold
NumericMatrix Rcpp__rmo_ex_arnold(const R_xlen_t n, const int d, const NumericVector& ex_intensities);
RcppExport SEXP _rmo_Rcpp__rmo_ex_arnold(SEXP nSEXP, SEXP dSEXP, SEXP ex_intensitiesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const R_xlen_t >::type n(nSEXP);
    Rcpp::traits::input_parameter< const int >::type d(dSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type ex_intensities(ex_intensitiesSEXP);
    rcpp_result_gen = Rcpp::wrap(Rcpp__rmo_ex_arnold(n, d, ex_intensities));
    return rcpp_result_gen;
END_RCPP
}
// Rcpp__rmo_esm_cuadras_auge
NumericMatrix Rcpp__rmo_esm_cuadras_auge(const R_xlen_t n, const int d, const double alpha, const double beta);
RcppExport SEXP _rmo_Rcpp__rmo_esm_cuadras_auge(SEXP nSEXP, SEXP dSEXP, SEXP alphaSEXP, SEXP betaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const R_xlen_t >::type n(nSEXP);
    Rcpp::traits::input_parameter< const int >::type d(dSEXP);
    Rcpp::traits::input_parameter< const double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< const double >::type beta(betaSEXP);
    rcpp_result_gen = Rcpp::wrap(Rcpp__rmo_esm_cuadras_auge(n, d, alpha, beta));
    return rcpp_result_gen;
END_RCPP
}
// Rcpp__rmo_lfm_cpp
NumericMatrix Rcpp__rmo_lfm_cpp(const R_xlen_t n, const R_xlen_t d, const double rate, const double rate_killing, const double rate_drift, const std::string rjump_name, const List& rjump_arg_list);
RcppExport SEXP _rmo_Rcpp__rmo_lfm_cpp(SEXP nSEXP, SEXP dSEXP, SEXP rateSEXP, SEXP rate_killingSEXP, SEXP rate_driftSEXP, SEXP rjump_nameSEXP, SEXP rjump_arg_listSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const R_xlen_t >::type n(nSEXP);
    Rcpp::traits::input_parameter< const R_xlen_t >::type d(dSEXP);
    Rcpp::traits::input_parameter< const double >::type rate(rateSEXP);
    Rcpp::traits::input_parameter< const double >::type rate_killing(rate_killingSEXP);
    Rcpp::traits::input_parameter< const double >::type rate_drift(rate_driftSEXP);
    Rcpp::traits::input_parameter< const std::string >::type rjump_name(rjump_nameSEXP);
    Rcpp::traits::input_parameter< const List& >::type rjump_arg_list(rjump_arg_listSEXP);
    rcpp_result_gen = Rcpp::wrap(Rcpp__rmo_lfm_cpp(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list));
    return rcpp_result_gen;
END_RCPP
}
// sample_cpp
NumericMatrix sample_cpp(const double rate, const double rate_killing, const double rate_drift, const std::string rjump_name, const List& rjump_arg_list, const NumericVector& barrier_values);
RcppExport SEXP _rmo_sample_cpp(SEXP rateSEXP, SEXP rate_killingSEXP, SEXP rate_driftSEXP, SEXP rjump_nameSEXP, SEXP rjump_arg_listSEXP, SEXP barrier_valuesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const double >::type rate(rateSEXP);
    Rcpp::traits::input_parameter< const double >::type rate_killing(rate_killingSEXP);
    Rcpp::traits::input_parameter< const double >::type rate_drift(rate_driftSEXP);
    Rcpp::traits::input_parameter< const std::string >::type rjump_name(rjump_nameSEXP);
    Rcpp::traits::input_parameter< const List& >::type rjump_arg_list(rjump_arg_listSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type barrier_values(barrier_valuesSEXP);
    rcpp_result_gen = Rcpp::wrap(sample_cpp(rate, rate_killing, rate_drift, rjump_name, rjump_arg_list, barrier_values));
    return rcpp_result_gen;
END_RCPP
}
// Rcpp__is_within
bool Rcpp__is_within(const R_xlen_t i, const R_xlen_t j);
RcppExport SEXP _rmo_Rcpp__is_within(SEXP iSEXP, SEXP jSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const R_xlen_t >::type i(iSEXP);
    Rcpp::traits::input_parameter< const R_xlen_t >::type j(jSEXP);
    rcpp_result_gen = Rcpp::wrap(Rcpp__is_within(i, j));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_rmo_Rcpp__rmo_esm", (DL_FUNC) &_rmo_Rcpp__rmo_esm, 3},
    {"_rmo_Rcpp__rmo_arnold", (DL_FUNC) &_rmo_Rcpp__rmo_arnold, 3},
    {"_rmo_Rcpp__rmo_ex_arnold", (DL_FUNC) &_rmo_Rcpp__rmo_ex_arnold, 3},
    {"_rmo_Rcpp__rmo_esm_cuadras_auge", (DL_FUNC) &_rmo_Rcpp__rmo_esm_cuadras_auge, 4},
    {"_rmo_Rcpp__rmo_lfm_cpp", (DL_FUNC) &_rmo_Rcpp__rmo_lfm_cpp, 7},
    {"_rmo_sample_cpp", (DL_FUNC) &_rmo_sample_cpp, 6},
    {"_rmo_Rcpp__is_within", (DL_FUNC) &_rmo_Rcpp__is_within, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_rmo(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
