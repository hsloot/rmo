#ifndef MO_STATS_WALKER_IMPL_UNIFCOUNTNOREPLACEWALKER_IPP
#define MO_STATS_WALKER_IMPL_UNIFCOUNTNOREPLACEWALKER_IPP

#include <cstddef>
#include <vector>
#include <numeric>

#include <mo/stats/walker.hpp>

namespace mo {
namespace stats {

template<typename RNGPolicy>
UnifCountNoReplaceWalker<RNGPolicy>::UnifCountNoReplaceWalker(const std::size_t n) :
    n_(n),
    values_(n) {
  std::iota(values_.begin(), values_.end(), 0);
}

template<typename RNGPolicy>
inline std::size_t UnifCountNoReplaceWalker<RNGPolicy>::operator()() {
  std::size_t index = rng_.R_unif_index(n_);
  --n_;
  std::size_t rval = values_[index];
  values_[index] = values_.back();
  values_.pop_back();
  n_ = values_.size();

  return rval;
}

} // stats
} // mo

#endif // MO_STATS_WALKER_IMPL_UNIFCOUNTNOREPLACEWALKER_IPP
