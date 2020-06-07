#ifndef MO_STATS_GENERATOR_IMPL_RUNIFORMINTGENERATOR_IPP
#define MO_STATS_GENERATOR_IMPL_RUNIFORMINTGENERATOR_IPP

#include <mo/stats/generator.hpp>
#include <Rinternals.h> // for R_xlen_t
#include <Rmath.h>

namespace mo {
namespace stats {

template<typename RNGPolicy>
UnifCountGenerator<RNGPolicy>::UnifCountGenerator(const R_xlen_t& n) :
    n_(n),
    rng_() {
  if (n_ < 1)
    std::range_error("n < 1");
}

template<typename RNGPolicy>
inline R_xlen_t UnifCountGenerator<RNGPolicy>::operator()() {
  return rng_.R_unif_index(n_);
}

} // stats
} // mo

#endif // MO_STATS_GENERATOR_IMPL_RUNIFORMINTGENERATOR_IPP
