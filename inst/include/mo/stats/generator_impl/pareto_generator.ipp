#ifndef MO_STATS_GENERATOR_IMPL_PARETOGENERATOR_IPP
#define MO_STATS_GENERATOR_IMPL_PARETOGENERATOR_IPP

#include <cmath>

#include <memory>
#include <utility>

#include <mo/stats/generator.hpp>

namespace mo {
namespace stats {

template<typename RNGPolicy>
ParetoGenerator<RNGPolicy>::ParetoGenerator(
    const double alpha, const double x0) :
    alpha_{ alpha },
    x0_{ x0 },
    rng_{} {}

template<typename RNGPolicy>
inline double ParetoGenerator<RNGPolicy>::operator()() {
  return (*this)(alpha_, x0_);
}

template<typename RNGPolicy>
inline double ParetoGenerator<RNGPolicy>::operator()(
    const double alpha, const double x0) {
  return x0 / std::pow(rng_.unif_rand(), 1/alpha);
}

template<typename RNGPolicy> // # nocov start
inline std::unique_ptr<RealUnivariateGenerator<double, RNGPolicy>>
ParetoGenerator<RNGPolicy>::clone() const {
  return std::move( std::unique_ptr<RealUnivariateGenerator<double, RNGPolicy>>(new ParetoGenerator<RNGPolicy>(*this)) );
} // # nocov end

} // stats
} // mo

#endif // MO_STATS_GENERATOR_IMPL_PARETOGENERATOR_IPP
