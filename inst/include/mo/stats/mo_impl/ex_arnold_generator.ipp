#ifndef MO_STATS_MO_IMPL_EX_ARNOLD_GENERATOR_IPP
#define MO_STATS_MO_IMPL_EX_ARNOLD_GENERATOR_IPP

#include <cstddef>
#include <vector>
#include <memory>

#include <mo/stats/mo.hpp>
#include <mo/math/misc.hpp>

namespace mo {
namespace stats {

template<typename Vector, typename RNGPolicy>
template<typename VectorIn>
ExArnoldGenerator<Vector, RNGPolicy>::ExArnoldGenerator(
    const std::size_t d, const VectorIn& ex_intensities) :
    d_(d),
    perm_generator_(d),
    wt_generators_(d),
    shock_generators_(d) {
  for (std::size_t i=0; i<d_; i++) {
    std::vector<double> intensities(d-i);
    for (std::size_t j=0; j<d-i; j++) {
      for (std::size_t k=0; k<i+1; k++) {
        intensities[j] += math::binomial_coefficient(i, k) * ex_intensities[k+j];
      }
      intensities[j] *= math::binomial_coefficient(d-i, j+1);
    }
    auto total_intensity = 0.;
    for (const auto intensity : intensities) total_intensity += intensity;
    wt_generators_[i].reset(new ExpGenerator<RNGPolicy>(total_intensity));
    shock_generators_[i].reset(new CountReplaceGenerator<RNGPolicy>(intensities));
  }
}


template<typename Vector, typename RNGPolicy>
void ExArnoldGenerator<Vector, RNGPolicy>::operator()(
    Vector& out) {
  std::vector<double> values(d_);
  std::size_t state = 0;
  while (state < d_) {
    auto waiting_time = (*wt_generators_[state])();
    for (std::size_t i=state; i<d_; i++) values[i] += waiting_time;
    state += 1 + (*shock_generators_[state])();
  }

  auto perm = perm_generator_();
  for (std::size_t i=0; i<d_; i++) out[i] = values[perm[i]];
}

}
}

#endif // MO_STATS_MO_IMPL_EX_ARNOLD_GENERATOR_IPP
