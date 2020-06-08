#ifndef MO_STATS_WALKER_IMPL_COUNTNOREPLACEWALKER_IPP
#define MO_STATS_WALKER_IMPL_COUNTNOREPLACEWALKER_IPP

#include <vector>
#include <R_ext/Utils.h>
#include <Rcpp.h>
#include <mo/stats/generator.hpp>
#include <mo/stats/walker.hpp>

namespace mo {
namespace stats {

template<typename RNGPolicy>
template<typename T>
CountNoReplaceWalker<RNGPolicy>::CountNoReplaceWalker(const T& probabilities) :
    n_(probabilities.size()),
    probabilities_(probabilities.begin(), probabilities.end()),
    original_order_(probabilities.size()),
    rng_() {
  for (const auto& probability : probabilities_)
    total_mass_ += probability;
  for (auto& probability : probabilities_)
    probability /= total_mass_;
  total_mass_ = 1.;
  std::iota(original_order_.begin(), original_order_.end(), 0);
  Rf_revsort(probabilities_.data(), original_order_.data(), (int) n_);
}


template<typename RNGPolicy>
inline R_xlen_t CountNoReplaceWalker<RNGPolicy>::operator()() {
  if (n_ == 0)
    std::runtime_error("Walker finished");

  auto rT = rng_.unif_rand() * total_mass_ ;
  auto mass = 0.;
  auto j = 0;
  for (; j<n_-1; j++) {
    mass += probabilities_[j];
    if (rT <= mass)
      break;
  }

  R_xlen_t rval = original_order_[j];
  total_mass_ -= probabilities_[j];
  probabilities_.erase(probabilities_.begin()+j);
  original_order_.erase(original_order_.begin()+j);
  n_--;

  return rval;
}

template<typename RNGPolicy>
inline std::unique_ptr<UnivariateWalker<R_xlen_t, RNGPolicy>> CountNoReplaceWalker<RNGPolicy>::clone() const {
  return std::move( std::unique_ptr<UnivariateWalker<R_xlen_t, RNGPolicy>>(new CountNoReplaceWalker<RNGPolicy>(*this)) );
}

} // stats
} // mo

#endif // MO_STATS_WALKER_IMPL_COUNTNOREPLACEWALKER_IPP