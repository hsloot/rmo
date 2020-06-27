#ifndef MO_STATS_WALKER_HPP
#define MO_STATS_WALKER_HPP

#include <cstddef> // for std::size_t
#include <vector>
#include <mo/stats/generator.hpp>
#include <mo/stats/rngpolicy.hpp>

namespace mo {
namespace stats {

class Walker {
public:
  virtual ~Walker() {}
}; // Walker

template<typename Scalar, typename RNGPolicy = RRNGPolicy>
class UnivariateWalker : public Walker {
public:
  virtual Scalar operator()();
}; // UnivariateWalker

template<typename Scalar, typename RNGPolicy = RRNGPolicy>
class UnivariateProcessWalker : public Walker {
public:
  struct ReturnValue {
    Scalar index;
    Scalar value;
  };
  virtual inline ReturnValue operator()();
}; // UnivariateProcessWalker

template<typename RNGPolicy = RRNGPolicy>
class CountNoReplaceWalker : public UnivariateWalker<std::size_t, RNGPolicy> {
public:
  CountNoReplaceWalker() = delete;
  CountNoReplaceWalker(const CountNoReplaceWalker& other) = default;
  CountNoReplaceWalker(CountNoReplaceWalker&& other) = default;
  template<typename T>
  CountNoReplaceWalker(const T& probabilities);

  virtual ~CountNoReplaceWalker() {}

  CountNoReplaceWalker& operator=(const CountNoReplaceWalker& other) = default;
  CountNoReplaceWalker& operator=(CountNoReplaceWalker&& other) = default;

  virtual inline std::size_t operator()() override final;

private:
  std::size_t n_;
  double total_mass_ = 0.;
  std::vector<double> probabilities_;
  std::vector<std::size_t> original_order_;

  RNGPolicy rng_;
}; // CountNoReplaceWalker

template<typename RNGPolicy = RRNGPolicy>
class UnifCountNoReplaceWalker : public UnivariateWalker<std::size_t, RNGPolicy>  {
public:
  UnifCountNoReplaceWalker() = delete;
  UnifCountNoReplaceWalker(const UnifCountNoReplaceWalker& other) = default;
  UnifCountNoReplaceWalker(UnifCountNoReplaceWalker&& other) = default;
  UnifCountNoReplaceWalker(const std::size_t n);

  virtual ~UnifCountNoReplaceWalker() {}

  UnifCountNoReplaceWalker& operator=(const UnifCountNoReplaceWalker& other) = default;
  UnifCountNoReplaceWalker& operator=(UnifCountNoReplaceWalker&& other) = default;

  virtual inline std::size_t operator()() override final;

private:
  std::size_t n_;
  std::vector<std::size_t> values_;

  RNGPolicy rng_;
}; // UnifCountNoReplaceWalker

} // stats
} // mo

#include <mo/stats/walker_impl/count_no_replace_walker.ipp>
#include <mo/stats/walker_impl/unif_count_no_replace_walker.ipp>

#endif // MO_STATS_WALKER_HPP
