#ifndef MO_STATS_GENERATOR_IMPL_REXPGENERATOR_IPP
#define MO_STATS_GENERATOR_IMPL_REXPGENERATOR_IPP

#include <Rcpp.h>
#include <mo/stats/generator.hpp>

namespace mo {
namespace stats {

template<typename RNGPolicy>
ExpGenerator<RNGPolicy>::ExpGenerator(const double& rate) :
    rate_(rate) {
  if (rate_ < 0.)
    std::range_error("rate < 0.");
}

template<typename RNGPolicy>
inline double ExpGenerator<RNGPolicy>::operator()() {
  return (*this)(rate_);
}

// WARNING: no check on rate
template<typename RNGPolicy>
inline double ExpGenerator<RNGPolicy>::operator()(const double& rate) {
  return 0. == rate ? R_PosInf : (R_PosInf == rate ? 0. : rng_.exp_rand() / rate);
}

} // stats
} // mo

#endif // MO_STATS_GENERATOR_IMPL_REXPGENERATOR_IPP
