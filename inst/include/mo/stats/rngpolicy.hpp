#ifndef MO_STATS_RNGPOLICY_HPP
#define MO_STATS_RNGPOLICY_HPP

#include <Rcpp.h>

namespace mo {
namespace stats {

class RNGPolicy {
public:
  virtual inline double unif_rand() = 0;
  virtual inline R_xlen_t R_unif_index(const R_xlen_t& n) = 0;
  virtual inline double exp_rand() = 0;
  virtual inline double norm_rand() = 0;
};

class RRNGPolicy : public RNGPolicy {
public:
  RRNGPolicy() = default;
  RRNGPolicy(const RRNGPolicy& other) = default;
  RRNGPolicy(RRNGPolicy&& other) = default;

  ~RRNGPolicy() {}

  RRNGPolicy& operator=(const RRNGPolicy& other) = default;
  RRNGPolicy& operator=(RRNGPolicy&& other) = default;

  virtual inline double unif_rand() override final;
  virtual inline R_xlen_t R_unif_index(const R_xlen_t& n) override final;
  virtual inline double exp_rand() override final;
  virtual inline double norm_rand() override final;
private:
  Rcpp::RNGScope rng_scope_;
};

} // stats
} // mo

#include <mo/stats/rngpolicy_impl/rrngpolicy.ipp>

#endif // MO_STATS_RNGPOLICY_HPP
