#ifndef MO_STATS_IMPLEMENTATION_FIXEDDBLGENERATOR_IPP
#define MO_STATS_IMPLEMENTATION_FIXEDDBLGENERATOR_IPP

#include <mo/stats/generator.hpp>

namespace mo {
namespace stats {

FixedDblGenerator::FixedDblGenerator(const double& value) :
    value_(value) {}

inline double FixedDblGenerator::operator()() const {
    return value_;
}

inline double FixedDblGenerator::operator()(const double& value) const {
    return value;
}

} // stats
} // mo

#endif // MO_STATS_IMPLEMENTATION_FIXEDDBLGENERATOR_IPP
