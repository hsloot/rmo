#ifndef MO_STATS_RNGPOLICY_IMPL_RRNGPOLICY_IPP
#define MO_STATS_RNGPOLICY_IMPL_RRNGPOLICY_IPP

#include <Rmath.h>
#include <mo/stats/rngpolicy.hpp>

namespace mo {
namespace stats {

double RRNGPolicy::unif_rand() {
  return static_cast<double>( ::unif_rand() );
}

R_xlen_t RRNGPolicy::R_unif_index(const R_xlen_t& n) {
  return static_cast<R_xlen_t>( ::R_unif_index(static_cast<double>(n)) );
}

double RRNGPolicy::exp_rand() {
  return static_cast<double>( ::exp_rand() );
}

double RRNGPolicy::norm_rand() {
  return static_cast<double>( ::norm_rand() );
}

} // stats
} // mo

#endif // MO_STATS_RNGPOLICY_IMPL_RRNGPOLICY_IPP
