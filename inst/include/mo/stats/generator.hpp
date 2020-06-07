#ifndef MO_STATS_GENERATOR_HPP
#define MO_STATS_GENERATOR_HPP

#include <vector>
#include <Rinternals.h> // for R_xlen_t

namespace mo {
namespace stats {

class Generator {
public:
  virtual ~Generator() = default;
}; // Generator

template<typename SCALAR>
class UnivariateGenerator : public Generator {
public:
  virtual inline SCALAR operator()() const = 0;
}; // UnivariateGenerator

template<typename VECTOR>
class MultivariateGenerator : public Generator {
public:
  virtual inline VECTOR operator()() const = 0;
  virtual inline void operator()(VECTOR& out) const = 0;
}; // MultivariateGenerator


class ExpGenerator : public UnivariateGenerator<double> {
public:
  virtual inline double operator()() const = 0;
  virtual inline double operator()(const double& rate) const = 0;
}; // ExpGenerator

class UnifGenerator : public UnivariateGenerator<double> {
public:

  virtual double operator()() const = 0;
}; // UnifGenerator

class IntGenerator : public UnivariateGenerator<R_xlen_t> {
public:
  virtual inline R_xlen_t operator()() const = 0;
}; // IntGenerator

class UnifIntGenerator : public UnivariateGenerator<R_xlen_t> {
public:
  virtual inline R_xlen_t operator()() const = 0;
}; // IntGenerator

class FixedDblGenerator : public UnivariateGenerator<double> {
public:
  FixedDblGenerator() = default;
  FixedDblGenerator(const FixedDblGenerator& other) = default;
  FixedDblGenerator(FixedDblGenerator&& other) = default;
  FixedDblGenerator(const double& value);

  FixedDblGenerator& operator=(const FixedDblGenerator& other) = default;
  FixedDblGenerator& operator=(FixedDblGenerator&& other) = default;

  virtual inline double operator()() const override final;
  virtual inline double operator()(const double& value) const final;
private:
  double value_ = 1.;
}; // FixedDblGenerator

template<typename VECTOR>
class PermutationGenerator : public MultivariateGenerator<VECTOR> {
public:
  virtual inline VECTOR operator()() const = 0;
  virtual inline void operator()(VECTOR& out) const = 0;
}; // PermutationGenerator


class RExpGenerator : public ExpGenerator {
public:
  RExpGenerator();
  RExpGenerator(const RExpGenerator& other) = default;
  RExpGenerator(RExpGenerator&& other) = default;
  RExpGenerator(double rate);

  RExpGenerator& operator=(const RExpGenerator& other) = default;
  RExpGenerator& operator=(RExpGenerator&& other) = default;

  virtual inline double operator()() const override final;
  virtual inline double operator()(const double& rate) const override final;

private:
  double rate_;
}; // RExpGenerator

class RUnifGenerator01 : public UnifGenerator {
public:
  RUnifGenerator01();
  RUnifGenerator01(const RUnifGenerator01& other) = default;
  RUnifGenerator01(RUnifGenerator01&& other) = default;

  RUnifGenerator01& operator=(const RUnifGenerator01& other) = default;
  RUnifGenerator01& operator=(RUnifGenerator01&& other) = default;

  virtual inline double operator()() const override final;
}; // RUnifGenerator

class RIntGenerator : public IntGenerator {
public:
  RIntGenerator() = delete;
  RIntGenerator(const RIntGenerator& other);
  RIntGenerator(RIntGenerator&& other) = default;
  template<typename T>
  RIntGenerator(const T& probabilities);

  RIntGenerator& operator=(const RIntGenerator& other);
  RIntGenerator& operator=(RIntGenerator&& other) = default;

  virtual inline R_xlen_t operator()() const override final;

private:
  std::vector<double> cumulative_probabilities_;
  std::vector<int> original_order_;
  std::unique_ptr<UnifGenerator> unif_generator_;
}; // RIntGenerator

class RUnifIntGenerator : public UnifIntGenerator {
public:
  RUnifIntGenerator() = delete;
  RUnifIntGenerator(const RUnifIntGenerator& other) = default;
  RUnifIntGenerator(RUnifIntGenerator&& other) = default;
  RUnifIntGenerator(const R_xlen_t& n);

  RUnifIntGenerator& operator=(const RUnifIntGenerator& other) = default;
  RUnifIntGenerator& operator=(RUnifIntGenerator&& other) = default;

  virtual inline R_xlen_t operator()() const override final;

private:
  R_xlen_t n_;
}; // RUniformIntGenerator

template<typename VECTOR>
class RPermutationGenerator : public PermutationGenerator<VECTOR> {
public:
  RPermutationGenerator();
  RPermutationGenerator(const RPermutationGenerator& other) = default;
  RPermutationGenerator(RPermutationGenerator&& other) = default;
  RPermutationGenerator(const R_xlen_t& n);

  RPermutationGenerator& operator=(const RPermutationGenerator& other) = default;
  RPermutationGenerator& operator=(RPermutationGenerator&& other) = default;

  virtual inline VECTOR operator()() const override final;
  virtual inline void operator()(VECTOR& out) const override final;

private:
  R_xlen_t n_ = 1;
};

} // stats
} // mo

#include <mo/stats/generator_impl/rexpgenerator.ipp>
#include <mo/stats/generator_impl/runifgenerator.ipp>
#include <mo/stats/generator_impl/rintgenerator.ipp>
#include <mo/stats/generator_impl/runifintgenerator.ipp>
#include <mo/stats/generator_impl/fixeddblgenerator.ipp>
#include <mo/stats/generator_impl/rpermutationgenerator.ipp>

#endif // MO_STATS_GENERATOR_HPP
