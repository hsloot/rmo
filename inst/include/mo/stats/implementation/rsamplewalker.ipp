#ifndef MO_STATS_IMPLEMENTATION_RSAMPLEWALKER_IPP
#define MO_STATS_IMPLEMENTATION_RSAMPLEWALKER_IPP

#include <vector>
#include <R_ext/Utils.h>
#include <Rcpp.h>
#include <mo/stats/generator.hpp>
#include <mo/stats/walker.hpp>

namespace mo {
namespace stats {

template<typename T>
RSampleWalker::RSampleWalker(const T& probabilities) :
    n_(probabilities.size()),
    probabilities_(probabilities.begin(), probabilities.end()),
    original_order_(probabilities.size()) {
  for (const auto& probability : probabilities_)
    total_mass_ += probability;
  for (auto& probability : probabilities_)
    probability /= total_mass_;
  total_mass_ = 1.;
  std::iota(original_order_.begin(), original_order_.end(), 0);
  Rf_revsort(probabilities_.data(), original_order_.data(), (int) n_);
}

inline R_xlen_t RSampleWalker::operator()() {
  if (n_ == 0)
    std::runtime_error("Walker finished");

  std::unique_ptr<UnifGenerator> unif_generator{new RUnifGenerator01()};
  auto rT = (*unif_generator)() * total_mass_ ;
  auto mass = 0.;
  auto j = 0;
  for (; j<n_-1; j++) {
    mass += probabilities_[j];
    if (rT <= mass)
      break;
  }

  auto rval = original_order_[j];
  total_mass_ -= probabilities_[j];
  probabilities_.erase(probabilities_.begin()+j);
  original_order_.erase(original_order_.begin()+j);
  n_--;


  return rval;
}

} // stats
} // mo

#endif // MO_STATS_IMPLEMENTATION_RSAMPLEWALKER_IPP
