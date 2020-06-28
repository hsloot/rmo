#ifndef MO_STATS_GENERATOR_IMPL_PERMUTATIONGENERATOR_IPP
#define MO_STATS_GENERATOR_IMPL_PERMUTATIONGENERATOR_IPP

#include <cstddef> // for std::size_t
#include <vector>
#include <numeric>

#include <mo/stats/generator.hpp>

namespace mo {
namespace stats {

template<typename Vector, typename RNGPolicy>
PermutationGenerator<Vector, RNGPolicy>::PermutationGenerator(const std::size_t n) :
    n_(n),
    rng_() {}

template<typename Vector, typename RNGPolicy>
inline void PermutationGenerator<Vector, RNGPolicy>::operator()(Vector& out) {
  std::vector<std::size_t> values(this->n_);
  std::iota(values.begin(), values.end(), 0);

  for (auto n=this->n_; n>0; n--) {
    std::size_t index = rng_.R_unif_index(n);
    out[this->n_-n] = values[index];
    values[index] = values.back();
    values.pop_back();
  }
}

template<typename Vector, typename RNGPolicy>
inline Vector PermutationGenerator<Vector, RNGPolicy>::operator()() {
  Vector out(this->n_);
  (*this)(out);
  return out;
}

} // stats
} // mo

#endif // MO_STATS_GENERATOR_IMPL_PERMUTATIONGENERATOR_IPP
