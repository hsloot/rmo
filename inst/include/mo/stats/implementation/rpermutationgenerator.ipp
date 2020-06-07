#ifndef MO_STATS_GENERATOR_IMPLEMENTATION_RPERMUTATIONGENERATOR_IPP
#define MO_STATS_GENERATOR_IMPLEMENTATION_RPERMUTATIONGENERATOR_IPP

#include <mo/stats/generator.hpp>

namespace mo {
namespace stats {

template<typename VECTOR>
RPermutationGenerator<VECTOR>::RPermutationGenerator() :
    RPermutationGenerator<VECTOR>(1) {}

template<typename VECTOR>
RPermutationGenerator<VECTOR>::RPermutationGenerator(const R_xlen_t& n) :
    n_(n) {}

template<typename VECTOR>
inline void RPermutationGenerator<VECTOR>::operator()(VECTOR& out) const {
  std::vector<R_xlen_t> values(this->n_);
  std::iota(values.begin(), values.end(), 0);

  for (auto n=this->n_; n>0; n--) {
    R_xlen_t index = static_cast<R_xlen_t>(::R_unif_index(n));
    out[this->n_-n] = values[index];
    values[index] = values.back();
    values.pop_back();
  }
}

template<typename VECTOR>
inline VECTOR RPermutationGenerator<VECTOR>::operator()() const {
  VECTOR out(this->n_);
  (*this)(out);
  return out;
}

} // stats
} // mo

#endif // MO_STATS_GENERATOR_IMPLEMENTATION_RPERMUTATIONGENERATOR_IPP
