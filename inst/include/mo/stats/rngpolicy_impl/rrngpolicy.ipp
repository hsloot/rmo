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
#if defined(R_VERSION) && R_VERSION >= R_Version(3, 3, 4)
  return static_cast<R_xlen_t>( ::R_unif_index(static_cast<double>(n)) );
#else
    /*
      Sample cannot be reimplemented fully backwards compatible because
      of logic changes in between R 3.3 and R 3.4. However, as long
      as the sample population and the number of samples are smaller
      than `INT_MAX`, this emulation should yield the same results.
     */
  return static_cast<R_xlen_t>( floor(n * ::unif_rand()) );
#endif
}

double RRNGPolicy::exp_rand() {
  return static_cast<double>( ::exp_rand() );
}

double RRNGPolicy::norm_rand() { // # nocov start
  return static_cast<double>( ::norm_rand() );
} // # nocov end

} // stats
} // mo

#endif // MO_STATS_RNGPOLICY_IMPL_RRNGPOLICY_IPP
