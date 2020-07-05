#ifndef MO_STATS_MO_IMPL_LFM_CPP_GENERATOR_IPP
#define MO_STATS_MO_IMPL_LFM_CPP_GENERATOR_IPP

#include <cstddef>
#include <vector>
#include <memory>
#include <limits>
#include <algorithm>

#include <mo/stats/mo.hpp>
#include <mo/utils/sort.hpp>

namespace mo {
namespace stats {

template<typename Vector, typename RNGPolicy>
LFMCPPGenerator<Vector, RNGPolicy>::LFMCPPGenerator(const LFMCPPGenerator<Vector, RNGPolicy>& other) :
    d_{ other.d_ },
    rate_drift_{ other.rate_drift_ },
    bv_generator_{ other.bv_generator_ },
    wt_generator_{ other.wt_generator_ },
    kt_generator_{ other.kt_generator_ },
    jump_generator_{ (*other.jump_generator_).clone() } {}

template<typename Vector, typename RNGPolicy>
LFMCPPGenerator<Vector, RNGPolicy>::LFMCPPGenerator(
    const std::size_t d, const double rate, const double rate_killing,
    const double rate_drift,  std::unique_ptr<RealUnivariateGenerator<double, RNGPolicy>>& jump_generator) :
    d_{ d },
    rate_drift_{ rate_drift },
    bv_generator_{},
    wt_generator_{ rate },
    kt_generator_{ rate_killing },
    jump_generator_{ (*jump_generator).clone() } {}

template<typename Vector, typename RNGPolicy>
LFMCPPGenerator<Vector, RNGPolicy>& LFMCPPGenerator<Vector, RNGPolicy>::operator=(const LFMCPPGenerator<Vector, RNGPolicy>& other) {
  if (this != &other) {
    d_ = other.d;
    rate_drift_ = other.rate_drift_;
    bv_generator_ = other.bv_generator_;
    wt_generator_ = other.wt_generator_;
    kt_generator_ = other.kt_generator_;
    jump_generator_ = (*other.jump_generator_).clone();
  }
  return *this;
}

template<typename Vector, typename RNGPolicy>
inline void LFMCPPGenerator<Vector, RNGPolicy>::operator()(Vector& out) {
  std::vector<double> barriers(d_);
  std::generate(barriers.begin(), barriers.end(), bv_generator_);
  auto killing_time = kt_generator_();

  auto time = 0.;
  auto value = 0.;
  auto idx = utils::sort_index(barriers);
  for (std::size_t i=0; i<d_; i++) {
    while (i<d_ && value < barriers[idx[i]]) {
      auto tt_jump = wt_generator_();
      auto tt_killing = killing_time - time;
      auto value_jump = (*jump_generator_)();

      if (
          tt_killing<std::numeric_limits<double>::infinity() &&
          tt_killing <= tt_jump) {
        while (i<d_ && rate_drift_ > 0. &&
            (barriers[idx[i]] - value)/rate_drift_ <= tt_killing) {
          auto tt_barrier = (barriers[idx[i]] - value) / rate_drift_;
          time += tt_barrier;
          value = barriers[idx[i]];
          tt_killing -= tt_barrier;
          out[idx[i++]] = time;
        }
        time = killing_time;
        value = std::numeric_limits<double>::infinity();
        while (i<d_) out[idx[i++]] = time;
      } else {
        while (i<d_ && rate_drift_ > 0. &&
            (barriers[idx[i]] - value)/rate_drift_ <= tt_jump) {
          auto tt_barrier = (barriers[idx[i]] - value)/rate_drift_;
          time += tt_barrier;
          value = barriers[idx[i]];
          tt_jump -= tt_barrier;
          out[idx[i++]] = time;
        }
        if (tt_jump < std::numeric_limits<double>::infinity()) {
          time += tt_jump;
          value += tt_jump * rate_drift_ + value_jump;
        }
        while (i<d_ && value >= barriers[idx[i]]) out[idx[i++]] = time;
      }
    }
  }
}

} // stats
} // mo

#endif // MO_STATS_MO_IMPL_LFM_CPP_GENERATOR_IPP
