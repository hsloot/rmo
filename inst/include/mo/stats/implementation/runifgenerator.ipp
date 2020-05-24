#ifndef MO_STATS_GENERATOR_IMPLEMENTATION_RUNIFGENERATOR01_IPP
#define MO_STATS_GENERATOR_IMPLEMENTATION_RUNIFGENERATOR01_IPP

#include <Rcpp.h>
#include <mo/stats/generator.hpp>

namespace mo {
namespace stats {

RUnifGenerator01::RUnifGenerator01() {}

inline double RUnifGenerator01::operator()() const {
  return R::unif_rand();
}

} // stats
} // mo

#endif // MO_STATS_GENERATOR_IMPLEMENTATION_RUNIFGENERATOR01_IPP
