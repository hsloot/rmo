#ifndef MO_STATS_GENERATOR_IMPL_COUNTREPLACEGENERATOR_IPP
#define MO_STATS_GENERATOR_IMPL_COUNTREPLACEGENERATOR_IPP

#include <cstddef>
#include <vector>
#include <numeric>
#include <algorithm>
#include <functional>

#include <mo/stats/generator.hpp>
#include <mo/utils/sort.hpp>

namespace mo {
namespace stats {

template<typename RNGPolicy>
template<typename Vector>
CountReplaceGenerator<RNGPolicy>::CountReplaceGenerator(
    const Vector& probabilities) :
    cumulative_probabilities_{ probabilities.begin(), probabilities.end() },
    original_order_(probabilities.size()),
    rng_{} {
  std::iota(original_order_.begin(), original_order_.end(), 0);

  utils::reverse_sort(cumulative_probabilities_, original_order_);

  std::partial_sum(cumulative_probabilities_.begin(),
    cumulative_probabilities_.end(), cumulative_probabilities_.begin());

  std::transform(
    cumulative_probabilities_.begin(), cumulative_probabilities_.end(),
    cumulative_probabilities_.begin(),
    std::bind(std::divides<double>(), std::placeholders::_1,
      cumulative_probabilities_.back()));
}

template<typename RNGPolicy>
inline std::size_t CountReplaceGenerator<RNGPolicy>::operator()() {
  auto u = rng_.unif_rand();
  auto it = std::lower_bound(
    cumulative_probabilities_.begin(), cumulative_probabilities_.end(), u);

  if (*it < u)
    throw std::logic_error("cumulative probabilities too small");

  return original_order_[it - cumulative_probabilities_.begin()];
}

} // stats
} // mo

#endif // MO_STATS_GENERATOR_IMPL_COUNTREPLACEGENERATOR_IPP
