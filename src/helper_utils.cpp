#include <Rcpp.h>
#include <rmo.hpp>

#include <R_ext/Utils.h>

using namespace Rcpp;
using namespace mo::utils;

// [[Rcpp::export]]
void mo_internal__reverse_sort(NumericVector& a, IntegerVector& ib) {
  reverse_sort(a, ib);
}

// [[Rcpp::export]]
void mo_internal__revsort_orig(NumericVector& a, IntegerVector& ib) {
  Rf_revsort(a.begin(), ib.begin(), static_cast<int>(a.size()));
}

// [[Rcpp::export]]
IntegerVector mo_internal__sort_index(NumericVector& x) {
  return wrap( sort_index(x) );
}
