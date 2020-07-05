#ifndef MO_STATS_MO_IMPL_CUADRAS_AUGE_GENERATOR_IPP
#define MO_STATS_MO_IMPL_CUADRAS_AUGE_GENERATOR_IPP

#include <limits>

#include <mo/stats/mo.hpp>
#include <mo/math/misc.hpp>

namespace mo {
namespace stats {

template<typename Vector, typename RNGPolicy>
CuadrasAugeGenerator<Vector, RNGPolicy>::CuadrasAugeGenerator(
    const std::size_t d, const double alpha, const double beta) :
    d_{ d },
    alpha_{ alpha },
    beta_{ beta },
    exp_generator_{} {}

template<typename Vector, typename RNGPolicy>
inline void CuadrasAugeGenerator<Vector, RNGPolicy>::operator()(Vector& out) {
  std::fill(out.begin(), out.end(), std::numeric_limits<double>::infinity());
  auto global_shock = exp_generator_(beta_);
  for (auto& value : out) {
    auto individual_shock = exp_generator_(alpha_);
    value = math::min(individual_shock, global_shock);
  }
}

} // stats
} // mo

#endif // MO_STATS_MO_IMPL_CUADRAS_AUGE_GENERATOR_IPP
