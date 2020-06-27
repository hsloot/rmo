#ifndef MO_STATS_GENERATOR_IMPL_EXPGENERATOR_IPP
#define MO_STATS_GENERATOR_IMPL_EXPGENERATOR_IPP

#include <Rcpp.h>
#include <mo/stats/generator.hpp>

namespace mo {
namespace stats {

template<typename RNGPolicy>
ExpGenerator<RNGPolicy>::ExpGenerator(const double rate) :
    rate_(rate) {
  if (rate_ < 0.)
    std::range_error("rate < 0."); // # nocov
}

template<typename RNGPolicy>
inline double ExpGenerator<RNGPolicy>::operator()() {
  return (*this)(rate_);
}

// WARNING: no check on rate
template<typename RNGPolicy>
inline double ExpGenerator<RNGPolicy>::operator()(const double rate) {
  return 0. == rate ? R_PosInf : (R_PosInf == rate ? 0. : rng_.exp_rand() / rate);
}

template<typename RNGPolicy> // # nocov start
inline std::unique_ptr<RealUnivariateGenerator<double, RNGPolicy>> ExpGenerator<RNGPolicy>::clone() const {
  return std::move( std::unique_ptr<RealUnivariateGenerator<double, RNGPolicy>>(new ExpGenerator<RNGPolicy>(*this)) );
} // # nocov end

template<typename RNGPolicy>
inline double ExpGenerator<RNGPolicy>::laplace(const double x) const {
  if (x < 0.)
    std::range_error("x < 0."); // # nocov

  return 0. == rate_ ? 0. : (R_PosInf == rate_ ? 1. : rate_/(rate_ + x));
}


} // stats
} // mo

#endif // MO_STATS_GENERATOR_IMPL_EXPGENERATOR_IPP
