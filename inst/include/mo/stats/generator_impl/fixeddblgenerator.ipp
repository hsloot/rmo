#ifndef MO_STATS_GENERATOR_IMPL_FIXEDDBLGENERATOR_IPP
#define MO_STATS_GENERATOR_IMPL_FIXEDDBLGENERATOR_IPP

#include <mo/stats/generator.hpp>

namespace mo {
namespace stats {

template<typename RNGPolicy>
FixedDblGenerator<RNGPolicy>::FixedDblGenerator(const double& value) :
    value_(value) {}

template<typename RNGPolicy>
inline double FixedDblGenerator<RNGPolicy>::operator()() {
    return value_;
}

template<typename RNGPolicy>
inline double FixedDblGenerator<RNGPolicy>::operator()(const double& value) {
    return value;
}

} // stats
} // mo

#endif // MO_STATS_GENERATOR_IMPL_FIXEDDBLGENERATOR_IPP
