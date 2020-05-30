#ifndef MO_STATS_WALKER_HPP
#define MO_STATS_WALKER_HPP

#include <vector>
#include <Rinternals.h> // for R_xlen_t
#include <mo/stats/generator.hpp>

namespace mo {
namespace stats {

class Walker {
public:
  virtual ~Walker() {}
}; // Walker

template<typename T>
class UnivariateWalker : public Walker {
public:
  virtual T operator()();
}; // UnivariateWalker

template<typename T, typename S>
class UnivariateProcessWalker : public Walker {
public:
  struct ReturnValue {
    S index;
    T value;
  };
  virtual inline ReturnValue operator()();
}; // UnivariateProcessWalker


class SampleWalkerNoReplace : public UnivariateWalker<R_xlen_t> {
public:
  virtual inline R_xlen_t operator()() = 0;
}; // SampleWalkerNoReplace

class UnifSampleWalkerNoReplace : public UnivariateWalker<R_xlen_t> {
public:
  virtual inline R_xlen_t operator()() = 0;
}; // UnifSampleWalkerNoReplace


class RSampleWalkerNoReplace : public SampleWalkerNoReplace {
public:
  RSampleWalkerNoReplace() = delete;
  RSampleWalkerNoReplace(const RSampleWalkerNoReplace& other);
  RSampleWalkerNoReplace(RSampleWalkerNoReplace&& other) = default;
  template<typename T>
  RSampleWalkerNoReplace(const T& probabilities);

  RSampleWalkerNoReplace& operator=(const RSampleWalkerNoReplace& other);
  RSampleWalkerNoReplace& operator=(RSampleWalkerNoReplace&& other) = default;

  virtual inline R_xlen_t operator()() override final;
private:
  R_xlen_t n_;
  double total_mass_ = 0.;
  std::vector<double> probabilities_;
  std::vector<int> original_order_;
  std::unique_ptr<UnifGenerator> unif_generator_;
}; // RSampleWalkerNoReplace

class RUnifSampleWalkerNoReplace : public UnifSampleWalkerNoReplace {
public:
  RUnifSampleWalkerNoReplace() = delete;
  RUnifSampleWalkerNoReplace(const RUnifSampleWalkerNoReplace& other) = default;
  RUnifSampleWalkerNoReplace(RUnifSampleWalkerNoReplace&& other) = default;
  RUnifSampleWalkerNoReplace(const R_xlen_t& n);

  RUnifSampleWalkerNoReplace& operator=(const RUnifSampleWalkerNoReplace& other) = default;
  RUnifSampleWalkerNoReplace& operator=(RUnifSampleWalkerNoReplace&& other) = default;

  virtual inline R_xlen_t operator()() override final;

private:
  R_xlen_t n_;
  std::vector<R_xlen_t> values_;
}; // RUnifSampleWalkerNoReplace

} // stats
} // mo

#include <mo/stats/implementation/rsamplewalkernoreplace.ipp>
#include <mo/stats/implementation/runifsamplewalkernoreplace.ipp>

#endif // MO_STATS_WALKER_HPP
