#ifndef MO_STATS_WALKER_HPP
#define MO_STATS_WALKER_HPP

#include <vector>
#include <Rcpp.h>

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


class RSampleWalkerNoReplace : public SampleWalkerNoReplace {
public:
  RSampleWalkerNoReplace() = delete;
  RSampleWalkerNoReplace(const RSampleWalkerNoReplace& other) = default;
  RSampleWalkerNoReplace(RSampleWalkerNoReplace&& other) = default;
  template<typename T>
  RSampleWalkerNoReplace(const T& probabilities);

  RSampleWalkerNoReplace& operator=(const RSampleWalkerNoReplace& other) = default;
  RSampleWalkerNoReplace& operator=(RSampleWalkerNoReplace&& other) = default;

  virtual inline R_xlen_t operator()() override final;
private:
  R_xlen_t n_;
  double total_mass_ = 0.;
  std::vector<double> probabilities_;
  std::vector<int> original_order_;
}; // RSampleWalkerNoReplace

} // stats
} // mo

#include <mo/stats/implementation/rsamplewalkernoreplace.ipp>

#endif // MO_STATS_WALKER_HPP
