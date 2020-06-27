#ifndef MO_STATS_GENERATOR_IMPL_UNIFCOUNTREPLACEGENERATOR_IPP
#define MO_STATS_GENERATOR_IMPL_UNIFCOUNTREPLACEGENERATOR_IPP

#include <cstddef> // for std::size_t
#include <mo/stats/generator.hpp>

namespace mo {
namespace stats {

template<typename RNGPolicy>
UnifCountReplaceGenerator<RNGPolicy>::UnifCountReplaceGenerator(const std::size_t n) :
    n_(n),
    rng_() {
  if (n_ < 1)
    std::range_error("n < 1");
}

template<typename RNGPolicy>
inline std::size_t UnifCountReplaceGenerator<RNGPolicy>::operator()() {
  return rng_.R_unif_index(n_);
}

} // stats
} // mo

#endif // MO_STATS_GENERATOR_IMPL_UNIFCOUNTREPLACEGENERATOR_IPP
