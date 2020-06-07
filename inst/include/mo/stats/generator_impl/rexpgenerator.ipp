#ifndef MO_STATS_GENERATOR_IMPL_REXPGENERATOR_IPP
#define MO_STATS_GENERATOR_IMPL_REXPGENERATOR_IPP

#include <Rcpp.h>
#include <mo/stats/generator.hpp>

namespace mo {
namespace stats {

RExpGenerator::RExpGenerator() :
    RExpGenerator(1.) {}

RExpGenerator::RExpGenerator(double rate) :
    rate_(rate) {
  if (rate_ < 0.)
    std::range_error("rate < 0.");
}

inline double RExpGenerator::operator()() const {
  return (*this)(rate_);
}

// WARNING: no check on rate
inline double RExpGenerator::operator()(const double& rate) const {
  return 0. == rate ? R_PosInf : (R_PosInf == rate ? 0. : R::exp_rand() / rate);
}

} // stats
} // mo

#endif // MO_STATS_GENERATOR_IMPL_REXPGENERATOR_IPP
