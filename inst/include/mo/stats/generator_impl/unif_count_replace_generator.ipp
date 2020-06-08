#ifndef MO_STATS_GENERATOR_IMPL_UNIFCOUNTREPLACEGENERATOR_IPP
#define MO_STATS_GENERATOR_IMPL_UNIFCOUNTREPLACEGENERATOR_IPP

#include <mo/stats/generator.hpp>
#include <Rinternals.h> // for R_xlen_t
#include <Rmath.h>

namespace mo {
namespace stats {

template<typename RNGPolicy>
UnifCountReplaceGenerator<RNGPolicy>::UnifCountReplaceGenerator(const R_xlen_t& n) :
    n_(n),
    rng_() {
  if (n_ < 1)
    std::range_error("n < 1");
}

template<typename RNGPolicy>
inline R_xlen_t UnifCountReplaceGenerator<RNGPolicy>::operator()() {
  return rng_.R_unif_index(n_);
}

template<typename RNGPolicy>
inline std::unique_ptr<UnivariateGenerator<R_xlen_t, RNGPolicy>> UnifCountReplaceGenerator<RNGPolicy>::clone() const {
  return std::move( std::unique_ptr<UnivariateGenerator<R_xlen_t, RNGPolicy>>(new UnifCountReplaceGenerator<RNGPolicy>(*this)) );
}

} // stats
} // mo

#endif // MO_STATS_GENERATOR_IMPL_UNIFCOUNTREPLACEGENERATOR_IPP
