#ifndef MO_STATS_WALKER_IMPL_COUNTNOREPLACEWALKER_IPP
#define MO_STATS_WALKER_IMPL_COUNTNOREPLACEWALKER_IPP

#include <cstddef> // for std::size_t
#include <vector>
#include <numeric>

#include <mo/stats/generator.hpp>
#include <mo/stats/walker.hpp>
#include <mo/utils/sort.hpp>

namespace mo {
namespace stats {

template<typename RNGPolicy>
template<typename Vector>
CountNoReplaceWalker<RNGPolicy>::CountNoReplaceWalker(const Vector& probabilities) :
    n_(probabilities.size()),
    total_mass_(0.),
    probabilities_(probabilities.begin(), probabilities.end()),
    original_order_(probabilities.size()),
    rng_() {
  for (const auto& probability : probabilities_)
    total_mass_ += probability;
  for (auto& probability : probabilities_)
    probability /= total_mass_;
  total_mass_ = 1.;
  std::iota(original_order_.begin(), original_order_.end(), 0);
  utils::reverse_sort(probabilities_, original_order_);
}


template<typename RNGPolicy>
inline std::size_t CountNoReplaceWalker<RNGPolicy>::operator()() {
  auto rT = rng_.unif_rand() * total_mass_ ;
  auto mass = 0.;
  auto j = 0;
  for (; j<n_-1; j++) {
    mass += probabilities_[j];
    if (rT <= mass)
      break;
  }

  std::size_t rval = original_order_[j];
  total_mass_ -= probabilities_[j];
  probabilities_.erase(probabilities_.begin()+j);
  original_order_.erase(original_order_.begin()+j);
  n_--;

  return rval;
}

} // stats
} // mo

#endif // MO_STATS_WALKER_IMPL_COUNTNOREPLACEWALKER_IPP
