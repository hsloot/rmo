#ifndef MO_STATS_WALKER_HPP
#define MO_STATS_WALKER_HPP

#include <cstddef> // for std::size_t

#include <mo/stats/generator.hpp>
#include <mo/stats/rngpolicy.hpp>

namespace mo {
namespace stats {

class Walker {
public:
  virtual ~Walker() = default;
}; // Walker

template<typename Scalar, typename RNGPolicy>
class UnivariateWalker : public Walker {
public:
  virtual Scalar operator()() = 0;
}; // UnivariateWalker

template<typename RNGPolicy>
class CountNoReplaceWalker : public UnivariateWalker<std::size_t, RNGPolicy> {
public:
  CountNoReplaceWalker() = delete;
  CountNoReplaceWalker(const CountNoReplaceWalker& other) = default;
  CountNoReplaceWalker(CountNoReplaceWalker&& other) = default;
  template<typename Vector>
  CountNoReplaceWalker(const Vector& probabilities);

  virtual ~CountNoReplaceWalker() = default;

  CountNoReplaceWalker& operator=(const CountNoReplaceWalker& other) = default;
  CountNoReplaceWalker& operator=(CountNoReplaceWalker&& other) = default;

  virtual std::size_t operator()() override final;

private:
  double total_mass_{ 1. };
  std::vector<double> probabilities_{};
  std::vector<std::size_t> original_order_{};

  RNGPolicy rng_{};
}; // CountNoReplaceWalker

template<typename RNGPolicy>
class UnifCountNoReplaceWalker : public UnivariateWalker<std::size_t, RNGPolicy>  {
public:
  UnifCountNoReplaceWalker() = delete;
  UnifCountNoReplaceWalker(const UnifCountNoReplaceWalker& other) = default;
  UnifCountNoReplaceWalker(UnifCountNoReplaceWalker&& other) = default;
  UnifCountNoReplaceWalker(const std::size_t n);

  virtual ~UnifCountNoReplaceWalker() = default;

  UnifCountNoReplaceWalker& operator=(const UnifCountNoReplaceWalker& other) = default;
  UnifCountNoReplaceWalker& operator=(UnifCountNoReplaceWalker&& other) = default;

  virtual std::size_t operator()() override final;

private:
  std::size_t n_{ 1 };
  std::vector<std::size_t> values_{};

  RNGPolicy rng_{};
}; // UnifCountNoReplaceWalker

} // stats
} // mo

#include <mo/stats/walker_impl/count_no_replace_walker.ipp>
#include <mo/stats/walker_impl/unif_count_no_replace_walker.ipp>

#endif // MO_STATS_WALKER_HPP
