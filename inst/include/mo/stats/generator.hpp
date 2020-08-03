#ifndef MO_STATS_GENERATOR_HPP
#define MO_STATS_GENERATOR_HPP

#include <cstddef>
#include <memory>
#include <vector>

#include <mo/stats/rngpolicy.hpp>

namespace mo {
namespace stats {

class Generator {
 public:
  virtual ~Generator() = default;
};  // Generator

template <typename Scalar, typename RNGPolicy>
class UnivariateGenerator : public Generator {
 public:
  virtual Scalar operator()() = 0;
};  // UnivariateGenerator

template <typename Vector, typename RNGPolicy>
class MultivariateGenerator : public Generator {
 public:
  virtual void operator()(Vector& out) = 0;
};  // MultivariateGenerator

template <typename Scalar, typename RNGPolicy>
class RealUnivariateGenerator : public UnivariateGenerator<Scalar, RNGPolicy> {
 public:
  virtual std::unique_ptr<RealUnivariateGenerator> clone() const = 0;
};

template <typename RNGPolicy>
class FixedDblGenerator : public RealUnivariateGenerator<double, RNGPolicy> {
 public:
  FixedDblGenerator() = default;
  FixedDblGenerator(const FixedDblGenerator& other) = default;
  FixedDblGenerator(FixedDblGenerator&& other) = default;
  explicit FixedDblGenerator(const double value);

  virtual ~FixedDblGenerator() = default;

  FixedDblGenerator& operator=(const FixedDblGenerator& other) = default;
  FixedDblGenerator& operator=(FixedDblGenerator&& other) = default;

  double operator()() final;
  inline double operator()(const double value);

  std::unique_ptr<RealUnivariateGenerator<double, RNGPolicy>> clone()
      const final;

 private:
  double value_{1.};
};  // FixedDblGenerator

template <typename RNGPolicy>
class ExpGenerator : public RealUnivariateGenerator<double, RNGPolicy> {
 public:
  ExpGenerator() = default;
  ExpGenerator(const ExpGenerator& other) = default;
  ExpGenerator(ExpGenerator&& other) = default;
  explicit ExpGenerator(const double rate);

  virtual ~ExpGenerator() = default;

  ExpGenerator& operator=(const ExpGenerator& other) = default;
  ExpGenerator& operator=(ExpGenerator&& other) = default;

  double operator()() final;
  inline double operator()(const double rate);

  std::unique_ptr<RealUnivariateGenerator<double, RNGPolicy>> clone()
      const final;

 private:
  double rate_{1.};

  RNGPolicy rng_{};
};  // ExpGenerator

template <typename RNGPolicy>
class ParetoGenerator : public RealUnivariateGenerator<double, RNGPolicy> {
 public:
  ParetoGenerator() = default;
  ParetoGenerator(const ParetoGenerator& other) = default;
  ParetoGenerator(ParetoGenerator&& other) = default;
  ParetoGenerator(const double alpha, const double x0);

  virtual ~ParetoGenerator() = default;

  ParetoGenerator& operator=(const ParetoGenerator& other) = default;
  ParetoGenerator& operator=(ParetoGenerator&& other) = default;

  double operator()() final;
  inline double operator()(const double alpha, const double x0);

  std::unique_ptr<RealUnivariateGenerator<double, RNGPolicy>> clone()
      const final;

 private:
  double alpha_{1.};
  double x0_{1.};

  RNGPolicy rng_{};
};  // ParetoGenerator

template <typename RNGPolicy>
class CountReplaceGenerator
    : public UnivariateGenerator<std::size_t, RNGPolicy> {
 public:
  CountReplaceGenerator() = delete;
  CountReplaceGenerator(const CountReplaceGenerator& other) = default;
  CountReplaceGenerator(CountReplaceGenerator&& other) = default;
  template <typename Vector>
  explicit CountReplaceGenerator(const Vector& probabilities);

  virtual ~CountReplaceGenerator() = default;

  CountReplaceGenerator& operator=(const CountReplaceGenerator& other) =
      default;
  CountReplaceGenerator& operator=(CountReplaceGenerator&& other) = default;

  std::size_t operator()() final;

 private:
  std::vector<double> cumulative_probabilities_{};
  std::vector<std::size_t> original_order_{};

  RNGPolicy rng_{};
};  // CountReplaceGenerator

template <typename RNGPolicy>
class UnifCountReplaceGenerator
    : public UnivariateGenerator<std::size_t, RNGPolicy> {
 public:
  UnifCountReplaceGenerator() = delete;
  UnifCountReplaceGenerator(const UnifCountReplaceGenerator& other) = default;
  UnifCountReplaceGenerator(UnifCountReplaceGenerator&& other) = default;
  explicit UnifCountReplaceGenerator(const std::size_t n);

  virtual ~UnifCountReplaceGenerator() = default;

  UnifCountReplaceGenerator& operator=(const UnifCountReplaceGenerator& other) =
      default;
  UnifCountReplaceGenerator& operator=(UnifCountReplaceGenerator&& other) =
      default;

  std::size_t operator()() final;

 private:
  std::size_t n_{1};

  RNGPolicy rng_{};
};  // UniformCountReplaceGenerator

template <typename Vector, typename RNGPolicy>
class UnifPermutationGenerator
    : public MultivariateGenerator<Vector, RNGPolicy> {
 public:
  UnifPermutationGenerator() = delete;
  UnifPermutationGenerator(const UnifPermutationGenerator& other) = default;
  UnifPermutationGenerator(UnifPermutationGenerator&& other) = default;
  explicit UnifPermutationGenerator(const std::size_t n);

  virtual ~UnifPermutationGenerator() = default;

  UnifPermutationGenerator& operator=(const UnifPermutationGenerator& other) =
      default;
  UnifPermutationGenerator& operator=(UnifPermutationGenerator&& other) =
      default;

  Vector operator()();
  void operator()(Vector& out) final;

 private:
  std::size_t n_{1};

  RNGPolicy rng_{};
};

}  // namespace stats
}  // namespace mo

#include <mo/stats/generator_impl/count_replace_generator.ipp>
#include <mo/stats/generator_impl/exp_generator.ipp>
#include <mo/stats/generator_impl/fixed_dbl_generator.ipp>
#include <mo/stats/generator_impl/pareto_generator.ipp>
#include <mo/stats/generator_impl/unif_count_replace_generator.ipp>
#include <mo/stats/generator_impl/unif_permutation_generator.ipp>

#endif  // MO_STATS_GENERATOR_HPP