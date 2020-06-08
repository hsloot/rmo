#ifndef MO_STATS_WALKER_IMPL_UNIFCOUNTNOREPLACEWALKER_IPP
#define MO_STATS_WALKER_IMPL_UNIFCOUNTNOREPLACEWALKER_IPP

#include <mo/stats/walker.hpp>
#include <Rinternals.h> // for R_xlen_t
#include <Rmath.h>

namespace mo {
namespace stats {

template<typename RNGPolicy>
UnifCountNoReplaceWalker<RNGPolicy>::UnifCountNoReplaceWalker(const R_xlen_t& n) :
    n_(n),
    values_(n) {
  if (n_ < 1)
    std::range_error("n < 1");
  std::iota(values_.begin(), values_.end(), 0);
}

template<typename RNGPolicy>
inline R_xlen_t UnifCountNoReplaceWalker<RNGPolicy>::operator()() {
  if (n_ == 0)
    std::runtime_error("Walker finished");
  R_xlen_t index = rng_.R_unif_index(n_);
  --n_;
  R_xlen_t rval = values_[index];
  values_[index] = values_.back();
  values_.pop_back();
  n_ = values_.size();

  return rval;
}

template<typename RNGPolicy>
inline std::unique_ptr<UnivariateWalker<R_xlen_t, RNGPolicy>> UnifCountNoReplaceWalker<RNGPolicy>::clone() const {
  return std::move( std::unique_ptr<UnivariateWalker<R_xlen_t, RNGPolicy>>(new UnifCountNoReplaceWalker<RNGPolicy>(*this)) );
}

} // stats
} // mo

#endif // MO_STATS_WALKER_IMPL_UNIFCOUNTNOREPLACEWALKER_IPP
