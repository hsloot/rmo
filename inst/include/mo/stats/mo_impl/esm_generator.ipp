#ifndef MO_STATS_MO_IMPL_ESM_GENERATOR_IPP
#define MO_STATS_MO_IMPL_ESM_GENERATOR_IPP

#include <algorithm>
#include <cstddef>

#include <mo/math/misc.hpp>
#include <mo/math/sets.hpp>
#include <mo/stats/mo.hpp>

namespace mo {
namespace stats {

template <typename Vector, typename RNGPolicy>
template <typename VectorIn>
ESMGenerator<Vector, RNGPolicy>::ESMGenerator(const std::size_t d,
                                              const VectorIn& intensities)
    : d_{d},
      intensities_{intensities.begin(), intensities.end()},
      exp_generator_{} {}

template <typename Vector, typename RNGPolicy>
inline void ESMGenerator<Vector, RNGPolicy>::operator()(Vector& out) {
  std::fill(out.begin(), out.end(), std::numeric_limits<double>::infinity());
  for (std::size_t j = 0, num_shocks = intensities_.size(); j < num_shocks;
       j++) {
    if (intensities_[j] > 0.) {
      auto shock_time = exp_generator_(intensities_[j]);
      for (std::size_t i = 0; i < d_; i++) {
        if (math::is_within(i, j)) {
          out[i] = math::min(out[i], shock_time);
        }
      }
    }
  }
}

}  // namespace stats
}  // namespace mo

#endif  // MO_STATS_MO_IMPL_ESM_GENERATOR_IPP
