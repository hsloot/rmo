// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(rmo)]]

#include <Rcpp.h>
#include <rmo.hpp>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector sort_index(const NumericVector x) {
  auto idx = mo::utils::sort_index(x);
  IntegerVector out(x.size());
  for (std::size_t i=0; i<idx.size(); i++)
    out[i] = idx[i];
  return out;
}

// [[Rcpp::export]]
void test(NumericVector x) {
  R_xlen_t i=0;
  while (i<x.size() && x[i] >= 0)
    Rcout << x[i++] << std::endl;
}

/*** R
x <- 1:5
sort_index(x)
order(x)-1

x <- c(2, 3, 8, 4, 2)
sort_index(x)
order(x)-1

test(1:5)
*/
