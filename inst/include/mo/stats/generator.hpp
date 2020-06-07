#ifndef MO_STATS_GENERATOR_HPP
#define MO_STATS_GENERATOR_HPP

#include <vector>
#include <Rinternals.h> // for R_xlen_t
#include <mo/stats/rngpolicy.hpp>

namespace mo {
namespace stats {

class Generator {
public:
  virtual ~Generator() = default;
}; // Generator

template<typename SCALAR, typename RNGPolicy = RRNGPolicy>
class UnivariateGenerator : public Generator {
public:
  virtual inline SCALAR operator()() = 0;
}; // UnivariateGenerator

template<typename VECTOR, typename RNGPolicy = RRNGPolicy>
class MultivariateGenerator : public Generator {
public:
  virtual inline VECTOR operator()() = 0;
  virtual inline void operator()(VECTOR& out) = 0;
}; // MultivariateGenerator

template<typename RNGPolicy = RRNGPolicy>
class FixedDblGenerator : public UnivariateGenerator<double, RNGPolicy> {
public:
  FixedDblGenerator() = default;
  FixedDblGenerator(const FixedDblGenerator& other) = default;
  FixedDblGenerator(FixedDblGenerator&& other) = default;
  FixedDblGenerator(const double& value);

  virtual ~FixedDblGenerator() {}

  FixedDblGenerator& operator=(const FixedDblGenerator& other) = default;
  FixedDblGenerator& operator=(FixedDblGenerator&& other) = default;

  virtual inline double operator()() override final;
  inline double operator()(const double& value);
private:
  double value_ = 1.;
}; // FixedDblGenerator

template<typename RNGPolicy = RRNGPolicy>
class ExpGenerator : public UnivariateGenerator<double, RNGPolicy> {
public:
  ExpGenerator() = default;
  ExpGenerator(const ExpGenerator& other) = default;
  ExpGenerator(ExpGenerator&& other) = default;
  ExpGenerator(const double& rate);

  virtual ~ExpGenerator() {}

  ExpGenerator& operator=(const ExpGenerator& other) = default;
  ExpGenerator& operator=(ExpGenerator&& other) = default;

  virtual inline double operator()() override final;
  inline double operator()(const double& rate);

private:
  double rate_ = 1.;

  RNGPolicy rng_;
}; // ExpGenerator

template<typename RNGPolicy = RRNGPolicy>
class CountReplaceGenerator : public UnivariateGenerator<R_xlen_t, RNGPolicy> {
public:
  CountReplaceGenerator() = delete;
  CountReplaceGenerator(const CountReplaceGenerator& other) = default;
  CountReplaceGenerator(CountReplaceGenerator&& other) = default;
  template<typename T>
  CountReplaceGenerator(const T& probabilities);

  virtual ~CountReplaceGenerator() {}

  CountReplaceGenerator& operator=(const CountReplaceGenerator& other) = default;
  CountReplaceGenerator& operator=(CountReplaceGenerator&& other) = default;

  virtual inline R_xlen_t operator()() override final;

private:
  std::vector<double> cumulative_probabilities_;
  std::vector<int> original_order_;

  RNGPolicy rng_;
}; // CountReplaceGenerator

template<typename RNGPolicy = RRNGPolicy>
class UnifCountReplaceGenerator : public UnivariateGenerator<R_xlen_t, RNGPolicy> {
public:
  UnifCountReplaceGenerator() = delete;
  UnifCountReplaceGenerator(const UnifCountReplaceGenerator& other) = default;
  UnifCountReplaceGenerator(UnifCountReplaceGenerator&& other) = default;
  UnifCountReplaceGenerator(const R_xlen_t& n);

  virtual ~UnifCountReplaceGenerator() {}

  UnifCountReplaceGenerator& operator=(const UnifCountReplaceGenerator& other) = default;
  UnifCountReplaceGenerator& operator=(UnifCountReplaceGenerator&& other) = default;

  virtual inline R_xlen_t operator()() override final;

private:
  R_xlen_t n_;

  RNGPolicy rng_;
}; // UniformCountReplaceGenerator


template<typename VECTOR, typename RNGPolicy = RRNGPolicy>
class PermutationGenerator : public MultivariateGenerator<VECTOR, RNGPolicy> {
public:
  PermutationGenerator() = delete;
  PermutationGenerator(const PermutationGenerator& other) = default;
  PermutationGenerator(PermutationGenerator&& other) = default;
  PermutationGenerator(const R_xlen_t& n);

  virtual ~PermutationGenerator() {}

  PermutationGenerator& operator=(const PermutationGenerator& other) = default;
  PermutationGenerator& operator=(PermutationGenerator&& other) = default;

  virtual inline VECTOR operator()() override final;
  virtual inline void operator()(VECTOR& out) override final;

private:
  R_xlen_t n_;

  RNGPolicy rng_;
};

} // stats
} // mo

#include <mo/stats/generator_impl/exp_generator.ipp>
#include <mo/stats/generator_impl/count_replace_generator.ipp>
#include <mo/stats/generator_impl/unif_count_replace_generator.ipp>
#include <mo/stats/generator_impl/fixed_dbl_generator.ipp>
#include <mo/stats/generator_impl/permutation_generator.ipp>

#endif // MO_STATS_GENERATOR_HPP
