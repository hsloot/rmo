#ifndef MO_STATS_WALKER_IMPL_COUNTNOREPLACEWALKER_IPP
#define MO_STATS_WALKER_IMPL_COUNTNOREPLACEWALKER_IPP

#include <cstddef>
#include <numeric>
#include <vector>

#include <mo/stats/generator.hpp>
#include <mo/stats/walker.hpp>
#include <mo/utils/sort.hpp>

namespace mo {
namespace stats {

template <typename RNGPolicy>
template <typename Vector>
CountNoReplaceWalker<RNGPolicy>::CountNoReplaceWalker(
    const Vector& probabilities)
    : total_mass_{1.},
      probabilities_{probabilities.begin(), probabilities.end()},
      original_order_(probabilities.size()),
      rng_{} {
  std::iota(original_order_.begin(), original_order_.end(), 0);
  std::transform(
      probabilities_.begin(), probabilities_.end(), probabilities_.begin(),
      std::bind(
          std::divides<double>(), std::placeholders::_1,
          std::accumulate(probabilities.begin(), probabilities.end(), 0.)));
  utils::reverse_sort(probabilities_, original_order_);
}

template <typename RNGPolicy>
inline std::size_t CountNoReplaceWalker<RNGPolicy>::operator()() {
  auto u = rng_.unif_rand() * total_mass_;
  auto mass = 0.;
  std::size_t j = 0;
  for (const auto& probability : probabilities_) {
    mass += probability;
    if (u <= mass) break;
    ++j;
  }
  auto out = original_order_[j];

  total_mass_ -= probabilities_[j];
  probabilities_.erase(probabilities_.begin() + j);
  original_order_.erase(original_order_.begin() + j);

  return out;
}

}  // namespace stats
}  // namespace mo

#endif  // MO_STATS_WALKER_IMPL_COUNTNOREPLACEWALKER_IPP
