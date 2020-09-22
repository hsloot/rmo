#ifndef MO_STATS_MO_IMPL_EX_ARNOLD_GENERATOR_IPP
#define MO_STATS_MO_IMPL_EX_ARNOLD_GENERATOR_IPP

#include <cstddef>
#include <memory>
#include <vector>

#include <mo/math/misc.hpp>
#include <mo/stats/mo.hpp>

namespace mo {
namespace stats {

inline std::vector<double> increment_ex_intensities_sm(
    const std::vector<double>& x) {
  std::vector<double> out(++x.begin(), x.end());
  std::transform(out.begin(), out.end(), x.begin(), out.begin(),
                 std::plus<double>());
  return out;
}

inline std::vector<double> scale_ex_intensities_sm(std::vector<double> x) {
  std::size_t j = 1;
  std::transform(x.begin(), x.end(), x.begin(), [&](double y) {
    return math::binomial_coefficient_factor(y, x.size(), (j++));
  });

  return x;
}

template <typename Vector, typename RNGPolicy>
template <typename VectorIn>
ExArnoldGenerator<Vector, RNGPolicy>::ExArnoldGenerator(
    const std::size_t d, const VectorIn& ex_intensities)
    : d_{d}, perm_generator_{d}, wt_generators_(d), shock_generators_(d) {
  std::vector<double> ex_intensities_sm(ex_intensities.begin(),
                                        ex_intensities.end());
  for (std::size_t i = 0; i < d_; i++) {
    if (0 < i)
      ex_intensities_sm = increment_ex_intensities_sm(ex_intensities_sm);

    std::vector<double> intensities =
        scale_ex_intensities_sm(ex_intensities_sm);

    auto total_intensity =
        std::accumulate(intensities.begin(), intensities.end(), 0.);
    wt_generators_[i].reset(new ExpGenerator<RNGPolicy>{total_intensity});
    shock_generators_[i].reset(
        new CountReplaceGenerator<RNGPolicy>{intensities});
  }
}

template <typename Vector, typename RNGPolicy>
inline void ExArnoldGenerator<Vector, RNGPolicy>::operator()(Vector& out) {
  std::vector<double> values(d_);
  std::size_t state = 0;
  while (state < d_) {
    auto waiting_time = (*wt_generators_[state])();
    for (std::size_t i = state; i < d_; i++) values[i] += waiting_time;
    state += 1 + (*shock_generators_[state])();
  }

  auto perm = perm_generator_();
  for (std::size_t i = 0; i < d_; i++) out[i] = values[perm[i]];
}

}  // namespace stats
}  // namespace mo

#endif  // MO_STATS_MO_IMPL_EX_ARNOLD_GENERATOR_IPP
