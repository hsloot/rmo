#ifndef MO_STATS_GENERATOR_HPP
#define MO_STATS_GENERATOR_HPP

namespace mo {
namespace stats {

class Generator {
public:
  virtual ~Generator() = default;
}; // Generator

class UnivariateGenerator : public Generator {
}; // UnivariateGenerator


class ExpGenerator : public UnivariateGenerator {
public:
  virtual double operator()() const = 0;
  virtual double operator()(const double& rate) const = 0;
}; // mo

class UnifGenerator : public UnivariateGenerator {
public:

  virtual double operator()() const = 0;
}; // mo


class RExpGenerator : public ExpGenerator {
public:
  RExpGenerator();
  RExpGenerator(const RExpGenerator& other) = default;
  RExpGenerator(RExpGenerator&& other) = default;
  RExpGenerator(double rate);

  RExpGenerator& operator=(const RExpGenerator& other) = default;
  RExpGenerator& operator=(RExpGenerator&& other) = default;

  virtual double operator()() const override;
  virtual double operator()(const double& rate) const override;
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

  virtual double operator()() const override; 
}; // RUnifGenerator

} // stats
} // mo

#include <mo/stats/implementation/rexpgenerator.ipp>
#include <mo/stats/implementation/runifgenerator.ipp>

#endif // MO_STATS_GENERATOR_HPP
