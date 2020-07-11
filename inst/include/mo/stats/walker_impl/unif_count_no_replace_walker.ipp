#ifndef MO_STATS_WALKER_IMPL_UNIFCOUNTNOREPLACEWALKER_IPP
#define MO_STATS_WALKER_IMPL_UNIFCOUNTNOREPLACEWALKER_IPP

#include <cstddef>
#include <numeric>
#include <vector>

#include <mo/stats/walker.hpp>

namespace mo {
namespace stats {

template <typename RNGPolicy>
UnifCountNoReplaceWalker<RNGPolicy>::UnifCountNoReplaceWalker(
    const std::size_t n)
    : n_{n}, values_(n), rng_{} {
  std::iota(values_.begin(), values_.end(), 0);
}

template <typename RNGPolicy>
inline std::size_t UnifCountNoReplaceWalker<RNGPolicy>::operator()() {
  auto j = rng_.R_unif_index(n_);
  auto out = values_[j];

  values_[j] = values_.back();
  values_.pop_back();
  --n_;

  return out;
}

}  // namespace stats
}  // namespace mo

#endif  // MO_STATS_WALKER_IMPL_UNIFCOUNTNOREPLACEWALKER_IPP
