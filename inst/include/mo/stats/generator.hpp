#ifndef MO_STATS_GENERATOR_HPP
#define MO_STATS_GENERATOR_HPP

#include <cstddef> // for std::size_t
#include <memory>
#include <vector>

#include <mo/stats/rngpolicy.hpp>

namespace mo {
namespace stats {

class Generator {
public:
  virtual ~Generator() = default;
}; // Generator


template<typename Scalar, typename RNGPolicy>
class UnivariateGenerator : public Generator {
public:
  virtual inline Scalar operator()() = 0;
}; // UnivariateGenerator

template<typename Vector, typename RNGPolicy>
class MultivariateGenerator : public Generator {
public:
  virtual inline Vector operator()() = 0;
  virtual inline void operator()(Vector& out) = 0;
}; // MultivariateGenerator


template<typename Scalar, typename RNGPolicy>
class RealUnivariateGenerator : public UnivariateGenerator<Scalar, RNGPolicy> {
public:
  virtual inline std::unique_ptr<RealUnivariateGenerator> clone() const = 0;

  virtual inline Scalar laplace(const Scalar x) const = 0;
};


template<typename RNGPolicy>
class FixedDblGenerator : public RealUnivariateGenerator<double, RNGPolicy> {
public:
  FixedDblGenerator() = default;
  FixedDblGenerator(const FixedDblGenerator& other) = default;
  FixedDblGenerator(FixedDblGenerator&& other) = default;
  FixedDblGenerator(const double value);

  virtual ~FixedDblGenerator() {}

  FixedDblGenerator& operator=(const FixedDblGenerator& other) = default;
  FixedDblGenerator& operator=(FixedDblGenerator&& other) = default;

  virtual inline double operator()() override final;
  inline double operator()(const double value);

  virtual inline std::unique_ptr<RealUnivariateGenerator<double, RNGPolicy>> clone() const override final;

  virtual inline double laplace(const double x) const override final;

private:
  double value_ = 1.;
}; // FixedDblGenerator

template<typename RNGPolicy>
class ExpGenerator : public RealUnivariateGenerator<double, RNGPolicy> {
public:
  ExpGenerator() = default;
  ExpGenerator(const ExpGenerator& other) = default;
  ExpGenerator(ExpGenerator&& other) = default;
  ExpGenerator(const double rate);

  virtual ~ExpGenerator() {}

  ExpGenerator& operator=(const ExpGenerator& other) = default;
  ExpGenerator& operator=(ExpGenerator&& other) = default;

  virtual inline double operator()() override final;
  inline double operator()(const double rate);

  virtual inline std::unique_ptr<RealUnivariateGenerator<double, RNGPolicy>> clone() const override final;

  virtual inline double laplace(const double x) const override final;

private:
  double rate_ = 1.;

  RNGPolicy rng_;
}; // ExpGenerator

template<typename RNGPolicy>
class CountReplaceGenerator : public UnivariateGenerator<std::size_t, RNGPolicy> {
public:
  CountReplaceGenerator() = delete;
  CountReplaceGenerator(const CountReplaceGenerator& other) = default;
  CountReplaceGenerator(CountReplaceGenerator&& other) = default;
  template<typename Vector>
  CountReplaceGenerator(const Vector& probabilities);

  virtual ~CountReplaceGenerator() {}

  CountReplaceGenerator& operator=(const CountReplaceGenerator& other) = default;
  CountReplaceGenerator& operator=(CountReplaceGenerator&& other) = default;

  virtual inline std::size_t operator()() override final;

private:
  std::vector<double> cumulative_probabilities_;
  std::vector<std::size_t> original_order_;

  RNGPolicy rng_;
}; // CountReplaceGenerator

template<typename RNGPolicy>
class UnifCountReplaceGenerator : public UnivariateGenerator<std::size_t, RNGPolicy> {
public:
  UnifCountReplaceGenerator() = delete;
  UnifCountReplaceGenerator(const UnifCountReplaceGenerator& other) = default;
  UnifCountReplaceGenerator(UnifCountReplaceGenerator&& other) = default;
  UnifCountReplaceGenerator(const std::size_t n);

  virtual ~UnifCountReplaceGenerator() {}

  UnifCountReplaceGenerator& operator=(const UnifCountReplaceGenerator& other) = default;
  UnifCountReplaceGenerator& operator=(UnifCountReplaceGenerator&& other) = default;

  virtual inline std::size_t operator()() override final;

private:
  std::size_t n_;

  RNGPolicy rng_;
}; // UniformCountReplaceGenerator


template<typename Vector, typename RNGPolicy>
class UnifPermutationGenerator : public MultivariateGenerator<Vector, RNGPolicy> {
public:
  UnifPermutationGenerator() = delete;
  UnifPermutationGenerator(const UnifPermutationGenerator& other) = default;
  UnifPermutationGenerator(UnifPermutationGenerator&& other) = default;
  UnifPermutationGenerator(const std::size_t n);

  virtual ~UnifPermutationGenerator() {}

  UnifPermutationGenerator& operator=(const UnifPermutationGenerator& other) = default;
  UnifPermutationGenerator& operator=(UnifPermutationGenerator&& other) = default;

  virtual inline Vector operator()() override final;
  virtual inline void operator()(Vector& out) override final;

private:
  std::size_t n_;

  RNGPolicy rng_;
};

} // stats
} // mo

#include <mo/stats/generator_impl/exp_generator.ipp>
#include <mo/stats/generator_impl/count_replace_generator.ipp>
#include <mo/stats/generator_impl/unif_count_replace_generator.ipp>
#include <mo/stats/generator_impl/fixed_dbl_generator.ipp>
#include <mo/stats/generator_impl/unif_permutation_generator.ipp>

#endif // MO_STATS_GENERATOR_HPP
