#ifndef MO_STATS_GENERATOR_IMPL_RINTGENERATOR_IPP
#define MO_STATS_GENERATOR_IMPL_RINTGENERATOR_IPP

#include <Rinternals.h> // for R_xlen_t
#include <R_ext/Utils.h> // for Rf_revsort
#include <mo/stats/generator.hpp>

namespace mo {
namespace stats {

template<typename RNGPolicy>
template<typename T>
CountGenerator<RNGPolicy>::CountGenerator(const T& probabilities) :
    cumulative_probabilities_(probabilities.begin(), probabilities.end()),
    original_order_(probabilities.size()),
    rng_() {
  // TODO: check that probabilities in not degenerated
  std::iota(original_order_.begin(), original_order_.end(), 0);
  auto n = cumulative_probabilities_.size();

  Rf_revsort(cumulative_probabilities_.data(), original_order_.data(), (int) n);
  for (R_xlen_t i=0; i<n; i++)
    cumulative_probabilities_[i] += (0==i ? 0. : cumulative_probabilities_[i-1]);
  auto total_mass = cumulative_probabilities_.back();
  for (R_xlen_t i=0; i<n; i++)
    cumulative_probabilities_[i] /= total_mass;
}

template<typename RNGPolicy>
inline R_xlen_t CountGenerator<RNGPolicy>::operator()() {
  auto rT = rng_.unif_rand();
  for (R_xlen_t j=0; j<cumulative_probabilities_.size(); j++) {
    if (cumulative_probabilities_[j] >= rT)
      return original_order_[j];
  }
  return cumulative_probabilities_.size();
}

} // stats
} // mo

#endif // MO_STATS_GENERATOR_IMPL_RINTGENERATOR_IPP
