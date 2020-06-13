#ifndef MO_STATS_GENERATOR_IMPL_FIXEDDBLGENERATOR_IPP
#define MO_STATS_GENERATOR_IMPL_FIXEDDBLGENERATOR_IPP

#include <mo/stats/generator.hpp>

namespace mo {
namespace stats {

template<typename RNGPolicy>
FixedDblGenerator<RNGPolicy>::FixedDblGenerator(const double& value) :
    value_(value) {
  if (value_ < 0.) // # nocov start
    std::range_error("value < 0."); // # nocov end
}

template<typename RNGPolicy>
inline double FixedDblGenerator<RNGPolicy>::operator()() {
    return value_;
}

template<typename RNGPolicy>
inline double FixedDblGenerator<RNGPolicy>::operator()(const double& value) {
    return value;
}

template<typename RNGPolicy> // # nocov start
inline std::unique_ptr<RealUnivariateGenerator<double, RNGPolicy>> FixedDblGenerator<RNGPolicy>::clone() const {
  return std::move( std::unique_ptr<RealUnivariateGenerator<double, RNGPolicy>>(new FixedDblGenerator<RNGPolicy>(*this)) );
} // # nocov end

template<typename RNGPolicy>
inline double FixedDblGenerator<RNGPolicy>::laplace(const double& x) const {
  if (x < 0.) // # nocov start
    std::range_error("x < 0."); // # nocov end
  return exp(-x*value_);
}

} // stats
} // mo

#endif // MO_STATS_GENERATOR_IMPL_FIXEDDBLGENERATOR_IPP
