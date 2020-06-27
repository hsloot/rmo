#include <limits>
#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
bool is_pos_inf(const double x) {
  return std::numeric_limits<double>::infinity() == x;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
is_pos_inf(0)
is_pos_inf(NA)
is_pos_inf(Inf)
is_pos_inf(-Inf)
is_pos_inf(1/0)
*/
