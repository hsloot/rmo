#ifndef MO_STATS_IMPLEMENTATION_RUNIFSAMPLEWALKERNOREPLACE_IPP
#define MO_STATS_IMPLEMENTATION_RUNIFSAMPLEWALKERNOREPLACE_IPP

#include <mo/stats/walker.hpp>
#include <Rinternals.h> // for R_xlen_t
#include <Rmath.h>

namespace mo {
namespace stats {

RUnifSampleWalkerNoReplace::RUnifSampleWalkerNoReplace(const R_xlen_t& n) :
    n_(n),
    values_(n) {
  if (n_ < 1)
    std::range_error("n < 1");
  std::iota(values_.begin(), values_.end(), 0);
}

inline R_xlen_t RUnifSampleWalkerNoReplace::operator()() {
  if (n_ == 0)
    std::runtime_error("Walker finished");
  R_xlen_t index = (R_xlen_t) ::R_unif_index(n_);
  --n_;
  R_xlen_t rval = values_[index];
  values_[index] = values_.back();
  values_.pop_back();
  n_ = values_.size();

  return rval;
}

} // stats
} // mo

#endif // MO_STATS_IMPLEMENTATION_RUNIFSAMPLEWALKERNOREPLACE_IPP
