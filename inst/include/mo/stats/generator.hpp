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
class CountGenerator : public UnivariateGenerator<R_xlen_t, RNGPolicy> {
public:
  CountGenerator() = delete;
  CountGenerator(const CountGenerator& other) = default;
  CountGenerator(CountGenerator&& other) = default;
  template<typename T>
  CountGenerator(const T& probabilities);

  virtual ~CountGenerator() {}

  CountGenerator& operator=(const CountGenerator& other) = default;
  CountGenerator& operator=(CountGenerator&& other) = default;

  virtual inline R_xlen_t operator()() override final;

private:
  std::vector<double> cumulative_probabilities_;
  std::vector<int> original_order_;

  RNGPolicy rng_;
}; // CountGenerator

template<typename RNGPolicy = RRNGPolicy>
class UnifCountGenerator : public UnivariateGenerator<R_xlen_t, RNGPolicy> {
public:
  UnifCountGenerator() = delete;
  UnifCountGenerator(const UnifCountGenerator& other) = default;
  UnifCountGenerator(UnifCountGenerator&& other) = default;
  UnifCountGenerator(const R_xlen_t& n);

  virtual ~UnifCountGenerator() {}

  UnifCountGenerator& operator=(const UnifCountGenerator& other) = default;
  UnifCountGenerator& operator=(UnifCountGenerator&& other) = default;

  virtual inline R_xlen_t operator()() override final;

private:
  R_xlen_t n_;

  RNGPolicy rng_;
}; // UniformCountGenerator


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

#include <mo/stats/generator_impl/rexpgenerator.ipp>
#include <mo/stats/generator_impl/rintgenerator.ipp>
#include <mo/stats/generator_impl/runifintgenerator.ipp>
#include <mo/stats/generator_impl/fixeddblgenerator.ipp>
#include <mo/stats/generator_impl/rpermutationgenerator.ipp>

#endif // MO_STATS_GENERATOR_HPP
