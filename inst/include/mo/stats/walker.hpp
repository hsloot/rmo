#ifndef MO_STATS_WALKER_HPP
#define MO_STATS_WALKER_HPP

#include <vector>
#include <Rinternals.h> // for R_xlen_t
#include <mo/stats/generator.hpp>
#include <mo/stats/rngpolicy.hpp>

namespace mo {
namespace stats {

class Walker {
public:
  virtual ~Walker() {}
}; // Walker

template<typename T, typename RNGPolicy = RRNGPolicy>
class UnivariateWalker : public Walker {
public:
  virtual T operator()();
}; // UnivariateWalker

template<typename T, typename S, typename RNGPolicy = RRNGPolicy>
class UnivariateProcessWalker : public Walker {
public:
  struct ReturnValue {
    S index;
    T value;
  };
  virtual inline ReturnValue operator()();
}; // UnivariateProcessWalker

template<typename RNGPolicy = RRNGPolicy>
class SampleWalkerNoReplace : public UnivariateWalker<R_xlen_t, RNGPolicy> {
public:
  SampleWalkerNoReplace() = delete;
  SampleWalkerNoReplace(const SampleWalkerNoReplace& other) = default;
  SampleWalkerNoReplace(SampleWalkerNoReplace&& other) = default;
  template<typename T>
  SampleWalkerNoReplace(const T& probabilities);

  virtual ~SampleWalkerNoReplace() {}

  SampleWalkerNoReplace& operator=(const SampleWalkerNoReplace& other) = default;
  SampleWalkerNoReplace& operator=(SampleWalkerNoReplace&& other) = default;

  virtual inline R_xlen_t operator()() override final;
private:
  R_xlen_t n_;
  double total_mass_ = 0.;
  std::vector<double> probabilities_;
  std::vector<int> original_order_;

  RNGPolicy rng_;
}; // SampleWalkerNoReplace

template<typename RNGPolicy = RRNGPolicy>
class UnifSampleWalkerNoReplace : public UnivariateWalker<R_xlen_t, RNGPolicy>  {
public:
  UnifSampleWalkerNoReplace() = delete;
  UnifSampleWalkerNoReplace(const UnifSampleWalkerNoReplace& other) = default;
  UnifSampleWalkerNoReplace(UnifSampleWalkerNoReplace&& other) = default;
  UnifSampleWalkerNoReplace(const R_xlen_t& n);

  virtual ~UnifSampleWalkerNoReplace() {}

  UnifSampleWalkerNoReplace& operator=(const UnifSampleWalkerNoReplace& other) = default;
  UnifSampleWalkerNoReplace& operator=(UnifSampleWalkerNoReplace&& other) = default;

  virtual inline R_xlen_t operator()() override final;

private:
  R_xlen_t n_;
  std::vector<R_xlen_t> values_;

  RNGPolicy rng_;
}; // UnifSampleWalkerNoReplace

} // stats
} // mo

#include <mo/stats/walker_impl/rsamplewalkernoreplace.ipp>
#include <mo/stats/walker_impl/runifsamplewalkernoreplace.ipp>

#endif // MO_STATS_WALKER_HPP
