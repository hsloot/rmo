#ifndef MO_STATS_GENERATOR_IMPL_COUNTREPLACEGENERATOR_IPP
#define MO_STATS_GENERATOR_IMPL_COUNTREPLACEGENERATOR_IPP

#include <cstddef> // for std::size_t
#include <mo/stats/generator.hpp>
#include <mo/utils/sort.hpp>

namespace mo {
namespace stats {

template<typename RNGPolicy>
template<typename T>
CountReplaceGenerator<RNGPolicy>::CountReplaceGenerator(const T& probabilities) :
    cumulative_probabilities_(probabilities.begin(), probabilities.end()),
    original_order_(probabilities.size()),
    rng_() {
  // TODO: check that probabilities in not degenerated
  std::iota(original_order_.begin(), original_order_.end(), 0);
  auto n = cumulative_probabilities_.size();

  utils::reverse_sort(cumulative_probabilities_, original_order_);
  for (std::size_t i=0; i<n; i++)
    cumulative_probabilities_[i] += (0==i ? 0. : cumulative_probabilities_[i-1]);
  auto total_mass = cumulative_probabilities_.back();
  for (std::size_t i=0; i<n; i++)
    cumulative_probabilities_[i] /= total_mass;
}

template<typename RNGPolicy>
inline std::size_t CountReplaceGenerator<RNGPolicy>::operator()() {
  auto rT = rng_.unif_rand();
  for (std::size_t j=0; j<cumulative_probabilities_.size(); j++) {
    if (cumulative_probabilities_[j] >= rT)
      return original_order_[j];
  }
  return cumulative_probabilities_.size();
}

} // stats
} // mo

#endif // MO_STATS_GENERATOR_IMPL_COUNTREPLACEGENERATOR_IPP
