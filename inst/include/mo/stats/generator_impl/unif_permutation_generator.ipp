#ifndef MO_STATS_GENERATOR_IMPL_PERMUTATIONGENERATOR_IPP
#define MO_STATS_GENERATOR_IMPL_PERMUTATIONGENERATOR_IPP

#include <cstddef>  // for std::size_t
#include <numeric>
#include <vector>

#include <mo/stats/generator.hpp>

namespace mo {
namespace stats {

template <typename Vector, typename RNGPolicy>
UnifPermutationGenerator<Vector, RNGPolicy>::UnifPermutationGenerator(
    const std::size_t n)
    : n_{n}, rng_{} {}

template <typename Vector, typename RNGPolicy>
inline void UnifPermutationGenerator<Vector, RNGPolicy>::operator()(
    Vector& out) {
  std::vector<std::size_t> values(n_);
  std::iota(values.begin(), values.end(), 0);

  for (auto it = out.begin(); it != out.end(); ++it) {
    auto value = values.begin() + rng_.R_unif_index(values.size());
    *it = *value;
    *value = values.back();
    values.pop_back();
  }
}

template <typename Vector, typename RNGPolicy>
inline Vector UnifPermutationGenerator<Vector, RNGPolicy>::operator()() {
  Vector out(n_);
  (*this)(out);
  return out;
}

}  // namespace stats
}  // namespace mo

#endif  // MO_STATS_GENERATOR_IMPL_PERMUTATIONGENERATOR_IPP
