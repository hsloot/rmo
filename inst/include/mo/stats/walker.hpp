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

template<typename SCALAR, typename RNGPolicy = RRNGPolicy>
class UnivariateWalker : public Walker {
public:
  virtual SCALAR operator()();

  virtual inline std::unique_ptr<UnivariateWalker> clone() const = 0;
}; // UnivariateWalker

template<typename TSCALAR, typename VSCALAR, typename RNGPolicy = RRNGPolicy>
class UnivariateProcessWalker : public Walker {
public:
  struct ReturnValue {
    TSCALAR index;
    VSCALAR value;
  };
  virtual inline ReturnValue operator()();

  virtual inline std::unique_ptr<UnivariateProcessWalker> clone() const = 0;
}; // UnivariateProcessWalker

template<typename RNGPolicy = RRNGPolicy>
class CountNoReplaceWalker : public UnivariateWalker<R_xlen_t, RNGPolicy> {
public:
  CountNoReplaceWalker() = delete;
  CountNoReplaceWalker(const CountNoReplaceWalker& other) = default;
  CountNoReplaceWalker(CountNoReplaceWalker&& other) = default;
  template<typename T>
  CountNoReplaceWalker(const T& probabilities);

  virtual ~CountNoReplaceWalker() {}

  CountNoReplaceWalker& operator=(const CountNoReplaceWalker& other) = default;
  CountNoReplaceWalker& operator=(CountNoReplaceWalker&& other) = default;

  virtual inline R_xlen_t operator()() override final;

  virtual inline std::unique_ptr<UnivariateWalker<R_xlen_t, RNGPolicy>> clone() const override final;

private:
  R_xlen_t n_;
  double total_mass_ = 0.;
  std::vector<double> probabilities_;
  std::vector<int> original_order_;

  RNGPolicy rng_;
}; // CountNoReplaceWalker

template<typename RNGPolicy = RRNGPolicy>
class UnifCountNoReplaceWalker : public UnivariateWalker<R_xlen_t, RNGPolicy>  {
public:
  UnifCountNoReplaceWalker() = delete;
  UnifCountNoReplaceWalker(const UnifCountNoReplaceWalker& other) = default;
  UnifCountNoReplaceWalker(UnifCountNoReplaceWalker&& other) = default;
  UnifCountNoReplaceWalker(const R_xlen_t& n);

  virtual ~UnifCountNoReplaceWalker() {}

  UnifCountNoReplaceWalker& operator=(const UnifCountNoReplaceWalker& other) = default;
  UnifCountNoReplaceWalker& operator=(UnifCountNoReplaceWalker&& other) = default;

  virtual inline R_xlen_t operator()() override final;

  virtual inline std::unique_ptr<UnivariateWalker<R_xlen_t, RNGPolicy>> clone() const override final;

private:
  R_xlen_t n_;
  std::vector<R_xlen_t> values_;

  RNGPolicy rng_;
}; // UnifCountNoReplaceWalker

} // stats
} // mo

#include <mo/stats/walker_impl/count_no_replace_walker.ipp>
#include <mo/stats/walker_impl/unif_count_no_replace_walker.ipp>

#endif // MO_STATS_WALKER_HPP
