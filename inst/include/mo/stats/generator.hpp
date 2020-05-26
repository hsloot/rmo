#ifndef MO_STATS_GENERATOR_HPP
#define MO_STATS_GENERATOR_HPP

#include <Rcpp.h>

namespace mo {
namespace stats {

class Generator {
public:
  virtual ~Generator() = default;
}; // Generator

template<typename T>
class UnivariateGenerator : public Generator {
public:
  virtual inline T operator()() const = 0;
}; // UnivariateGenerator

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
  template<typename T>
  RIntGenerator(const T& probabilities);
  RIntGenerator(const RIntGenerator& other);
  RIntGenerator(RIntGenerator&& other) = default;

  RIntGenerator& operator=(const RIntGenerator& other);
  RIntGenerator& operator=(RIntGenerator&& other) = default;

  virtual inline R_xlen_t operator()() const override final;

private:
  std::vector<double> cumulative_probabilities_;
  std::vector<int> original_order_;
  std::unique_ptr<UnifGenerator> unif_generator_;
}; // RIntGenerator

} // stats
} // mo

#include <mo/stats/implementation/rexpgenerator.ipp>
#include <mo/stats/implementation/runifgenerator.ipp>
#include <mo/stats/implementation/rintgenerator.ipp>
#include <mo/stats/implementation/fixeddblgenerator.ipp>

#endif // MO_STATS_GENERATOR_HPP
