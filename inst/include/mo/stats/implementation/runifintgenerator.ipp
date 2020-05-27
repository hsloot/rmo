#ifndef MO_STATS_IMPLEMENTATION_RUNIFORMINTGENERATOR_IPP
#define MO_STATS_IMPLEMENTATION_RUNIFORMINTGENERATOR_IPP

#include <mo/stats/generator.hpp>
#include <Rinternals.h> // for R_xlen_t
#include <Rmath.h>

namespace mo {
namespace stats {

RUnifIntGenerator::RUnifIntGenerator(const R_xlen_t& n) :
    n_(n) {
  if (n_ < 1)
    std::range_error("n < 1");
}

inline R_xlen_t RUnifIntGenerator::operator()() const {
  return ::R_unif_index(n_);
}

} // stats
} // mo

#endif // MO_STATS_IMPLEMENTATION_RUNIFORMINTGENERATOR_IPP
