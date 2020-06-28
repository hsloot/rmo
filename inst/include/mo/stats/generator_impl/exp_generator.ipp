#ifndef MO_STATS_GENERATOR_IMPL_EXPGENERATOR_IPP
#define MO_STATS_GENERATOR_IMPL_EXPGENERATOR_IPP

#include <memory>
#include <utility>

#include <mo/stats/generator.hpp>

namespace mo {
namespace stats {

template<typename RNGPolicy>
ExpGenerator<RNGPolicy>::ExpGenerator(const double rate) :
    rate_(rate) {}

template<typename RNGPolicy>
inline double ExpGenerator<RNGPolicy>::operator()() {
  return (*this)(rate_);
}

// WARNING: no check on rate
template<typename RNGPolicy>
inline double ExpGenerator<RNGPolicy>::operator()(const double rate) {
  if (0. == rate) return std::numeric_limits<double>::infinity();
  else if (std::numeric_limits<double>::infinity() == rate) return 0.;
  else return rng_.exp_rand() / rate;
}

template<typename RNGPolicy> // # nocov start
inline std::unique_ptr<RealUnivariateGenerator<double, RNGPolicy>> ExpGenerator<RNGPolicy>::clone() const {
  return std::move( std::unique_ptr<RealUnivariateGenerator<double, RNGPolicy>>(new ExpGenerator<RNGPolicy>(*this)) );
} // # nocov end

template<typename RNGPolicy>
inline double ExpGenerator<RNGPolicy>::laplace(const double x) const {
  if (0. == rate_) return 0.;
  else if (std::numeric_limits<double>::infinity() == rate_) return 1.;
  else return rate_/(rate_ + x);
}


} // stats
} // mo

#endif // MO_STATS_GENERATOR_IMPL_EXPGENERATOR_IPP
