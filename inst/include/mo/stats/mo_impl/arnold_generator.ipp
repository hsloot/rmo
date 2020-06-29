#ifndef MO_STATS_MO_IMPL_ARNOLD_GENERATOR_IPP
#define MO_STATS_MO_IMPL_ARNOLD_GENERATOR_IPP

#include <cstddef> // for std::size_t
#include <vector>
#include <numeric>
#include <algorithm>

#include <mo/stats/mo.hpp>
#include <mo/math/sets.hpp>

namespace mo {
namespace stats {

template<typename Vector, typename RNGPolicy>
template<typename VectorIn>
ArnoldGenerator<Vector, RNGPolicy>::ArnoldGenerator(
    const std::size_t d, const VectorIn& intensities) :
    d_(d),
    wt_generator_(std::accumulate(intensities.begin(), intensities.end(), 0.)),
    shock_generator_(intensities) {}

template<typename Vector, typename RNGPolicy>
void ArnoldGenerator<Vector, RNGPolicy>::operator()(
    Vector& out) {
  std::fill(out.begin(), out.end(), 0.);
  std::vector<bool> destroyed(d_, false);

  while (!std::all_of(
      destroyed.begin(), destroyed.end(), [](bool v) { return v; })) {
    auto waiting_time = wt_generator_();
    auto affected = shock_generator_();

    for (std::size_t i=0; i<d_; i++) {
      if (!destroyed[i]) {
        out[i] += waiting_time;
        if (math::is_within(i, affected)) destroyed[i] = true;
      }
    }
  }
}

}
}

#endif
