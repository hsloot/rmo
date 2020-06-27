#ifndef MO_STATS_RNGPOLICY_HPP
#define MO_STATS_RNGPOLICY_HPP

#include <cstddef> // for std::size_t
#include <Rcpp.h>

namespace mo {
namespace stats {

class RNGPolicy {
public:
  virtual inline double unif_rand() = 0;
  virtual inline std::size_t R_unif_index(const std::size_t n) = 0;
  virtual inline double exp_rand() = 0;
  virtual inline double norm_rand() = 0;
};

#ifdef RMATH_H
class RRNGPolicy : public RNGPolicy {
public:
  RRNGPolicy() = default;
  RRNGPolicy(const RRNGPolicy& other) = default;
  RRNGPolicy(RRNGPolicy&& other) = default;

  ~RRNGPolicy() {}

  RRNGPolicy& operator=(const RRNGPolicy& other) = default;
  RRNGPolicy& operator=(RRNGPolicy&& other) = default;

  virtual inline double unif_rand() override final;
  virtual inline std::size_t R_unif_index(const std::size_t n) override final;
  virtual inline double exp_rand() override final;
  virtual inline double norm_rand() override final;
};
#endif // RMATH_H

} // stats
} // mo

#ifdef RMATH_H
#include <mo/stats/rngpolicy_impl/rrngpolicy.ipp>
#endif // RMATH_H

#endif // MO_STATS_RNGPOLICY_HPP
