#ifndef RMO_MATH_H_
#define RMO_MATH_H_

#include <Rcpp.h>

inline double min2(double a, double b) {
  if (a == R_PosInf && b == R_PosInf) {
    return R_PosInf;
  } else if (a == R_PosInf) {
    return b;
  } else if (b == R_PosInf) {
    return a;
  }
  return (a < b ? a : b);
} // inline double min2(double a, double b);

#endif
