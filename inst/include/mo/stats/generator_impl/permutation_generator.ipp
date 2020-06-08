#ifndef MO_STATS_GENERATOR_IMPL_PERMUTATIONGENERATOR_IPP
#define MO_STATS_GENERATOR_IMPL_PERMUTATIONGENERATOR_IPP

#include <mo/stats/generator.hpp>

namespace mo {
namespace stats {

template<typename VECTOR, typename RNGPolicy>
PermutationGenerator<VECTOR, RNGPolicy>::PermutationGenerator(const R_xlen_t& n) :
    n_(n),
    rng_() {}

template<typename VECTOR, typename RNGPolicy>
inline void PermutationGenerator<VECTOR, RNGPolicy>::operator()(VECTOR& out) {
  std::vector<R_xlen_t> values(this->n_);
  std::iota(values.begin(), values.end(), 0);

  for (auto n=this->n_; n>0; n--) {
    R_xlen_t index = rng_.R_unif_index(n);
    out[this->n_-n] = values[index];
    values[index] = values.back();
    values.pop_back();
  }
}

template<typename VECTOR, typename RNGPolicy>
inline VECTOR PermutationGenerator<VECTOR, RNGPolicy>::operator()() {
  VECTOR out(this->n_);
  (*this)(out);
  return out;
}

template<typename VECTOR, typename RNGPolicy>
inline std::unique_ptr<MultivariateGenerator<VECTOR, RNGPolicy>> PermutationGenerator<VECTOR, RNGPolicy>::clone() const {
  return std::move( std::unique_ptr<MultivariateGenerator<VECTOR, RNGPolicy>>(new PermutationGenerator<VECTOR, RNGPolicy>(*this)) );
}

} // stats
} // mo

#endif // MO_STATS_GENERATOR_IMPL_PERMUTATIONGENERATOR_IPP
