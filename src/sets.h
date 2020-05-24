#ifndef RMO_SETS_H_
#define RMO_SETS_H_

#include <Rcpp.h>

inline bool is_within(const R_xlen_t &i, const R_xlen_t &j)  {
  return (j >> (i-1)) % 2 == 1;
} // inline bool is_within(const R_xlen_t &i, const R_xlen_t &j);

#endif
