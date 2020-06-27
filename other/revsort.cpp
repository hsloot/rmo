// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(rmo)]]

#include <R_ext/Utils.h>
#include <Rcpp.h>
#include <mo.hpp>
using namespace Rcpp;


// [[Rcpp::export]]
void Rf_revsort(NumericVector& a, IntegerVector& ib, int n) {
  Rf_revsort(&a[0], &ib[0], n);
}

// [[Rcpp::export]]
void reverse_sort(NumericVector& a, IntegerVector& ib, int n) {
  mo::utils::reverse_sort(a, ib);
}

// [[Rcpp::export]]
void reverse_sort2(NumericVector& a, IntegerVector& ib, int n) {
  std::vector<std::size_t> ib_ = as<std::vector<std::size_t>>(ib);
  mo::utils::reverse_sort(a, ib_);
  for (std::size_t i = 0; i < ib.size(); i++) ib[i] = ib_[i];
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
a <- c(1, 2, 6, 3 , 2)
n <- length(a)
ib <- seq(1, length(a))
Rf_revsort(a, ib, n)
a
ib

a <- c(1, 2, 6, 3 , 2)
n <- length(a)
ib <- seq(1, length(a))
reverse_sort(a, ib, n)
a
ib

a <- c(1, 2, 6, 3 , 2)
n <- length(a)
ib <- seq(1, length(a))
reverse_sort2(a, ib, n)
a
ib


a <- c(1, 1, 1, 1, 1, 1, 1)
n <- length(a)
ib <- seq(1, length(a))
Rf_revsort(a, ib, n)
a
ib

a <- c(1, 1, 1, 1, 1, 1, 1)
n <- length(a)
ib <- seq(1, length(a))
reverse_sort(a, ib, n)
a
ib

a <- c(1, 1, 1, 1, 1, 1, 1)
n <- length(a)
ib <- seq(1, length(a))
reverse_sort2(a, ib, n)
a
ib



a <- c(8, 7, 3, 10, 6, 1, 2, 9, 5, 4)
n <- length(a)
ib <- seq(1, length(a))
Rf_revsort(a, ib, n)
a
ib

a <- c(8, 7, 3, 10, 6, 1, 2, 9, 5, 4)
n <- length(a)
ib <- seq(1, length(a))
reverse_sort(a, ib, n)
a
ib

a <- c(8, 7, 3, 10, 6, 1, 2, 9, 5, 4)
n <- length(a)
ib <- seq(1, length(a))
reverse_sort2(a, ib, n)
a
ib
*/
