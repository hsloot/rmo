#ifndef MO_STATS_MO_HPP
#define MO_STATS_MO_HPP

#include <cstddef>
#include <memory>
#include <vector>

#include <mo/stats/generator.hpp>
#include <mo/stats/rngpolicy.hpp>

namespace mo {
namespace stats {

template <typename Vector, typename RNGPolicy>
class ESMGenerator : public MultivariateGenerator<Vector, RNGPolicy> {
 public:
  ESMGenerator() = delete;
  ESMGenerator(const ESMGenerator& other) = default;
  ESMGenerator(ESMGenerator&& other) = default;

  template <typename VectorIn>
  ESMGenerator(const std::size_t d, const VectorIn& intensities);

  virtual ~ESMGenerator() = default;

  ESMGenerator& operator=(const ESMGenerator& other) = default;
  ESMGenerator& operator=(ESMGenerator&& other) = default;

  void operator()(Vector& out) final;

 private:
  std::size_t d_{2};
  std::vector<double> intensities_{};
  ExpGenerator<RNGPolicy> exp_generator_{};
};  // ESMGenerator

template <typename Vector, typename RNGPolicy>
class CuadrasAugeGenerator : public MultivariateGenerator<Vector, RNGPolicy> {
 public:
  CuadrasAugeGenerator() = delete;
  CuadrasAugeGenerator(const CuadrasAugeGenerator& other) = default;
  CuadrasAugeGenerator(CuadrasAugeGenerator&& other) = default;

  CuadrasAugeGenerator(const std::size_t d, const double alpha,
                       const double beta);

  virtual ~CuadrasAugeGenerator() = default;

  CuadrasAugeGenerator& operator=(const CuadrasAugeGenerator& other) = default;
  CuadrasAugeGenerator& operator=(CuadrasAugeGenerator&& other) = default;

  void operator()(Vector& out) final;

 private:
  std::size_t d_{2};
  double alpha_{1.};
  double beta_{1.};
  ExpGenerator<RNGPolicy> exp_generator_{};
};  // CuadrasAugeGenerator

template <typename Vector, typename RNGPolicy>
class ArnoldGenerator : public MultivariateGenerator<Vector, RNGPolicy> {
 public:
  ArnoldGenerator() = delete;
  ArnoldGenerator(const ArnoldGenerator& other) = default;
  ArnoldGenerator(ArnoldGenerator&& other) = default;

  template <typename VectorIn>
  ArnoldGenerator(const std::size_t d, const VectorIn& intensities);

  virtual ~ArnoldGenerator() = default;

  ArnoldGenerator& operator=(const ArnoldGenerator& other) = default;
  ArnoldGenerator& operator=(ArnoldGenerator&& other) = default;

  void operator()(Vector& out) final;

 private:
  std::size_t d_{2};
  ExpGenerator<RNGPolicy> wt_generator_{};
  CountReplaceGenerator<RNGPolicy> shock_generator_{};
};  // ArnoldGenerator

template <typename Vector, typename RNGPolicy>
class ExArnoldGenerator : public MultivariateGenerator<Vector, RNGPolicy> {
 public:
  ExArnoldGenerator() = delete;
  ExArnoldGenerator(const ExArnoldGenerator& other) = default;
  ExArnoldGenerator(ExArnoldGenerator&& other) = default;

  template <typename VectorIn>
  ExArnoldGenerator(const std::size_t d, const VectorIn& ex_intensities);

  virtual ~ExArnoldGenerator() = default;

  ExArnoldGenerator& operator=(const ExArnoldGenerator& other) = default;
  ExArnoldGenerator& operator=(ExArnoldGenerator&& other) = default;

  void operator()(Vector& out) final;

 private:
  std::size_t d_{2};
  UnifPermutationGenerator<std::vector<std::size_t>, RNGPolicy>
      perm_generator_{};
  std::vector<std::unique_ptr<ExpGenerator<RNGPolicy>>> wt_generators_{};
  std::vector<std::unique_ptr<CountReplaceGenerator<RNGPolicy>>>
      shock_generators_{};
};  // ExArnoldGenerator

template <typename Vector, typename RNGPolicy>
class LFMCPPGenerator : public MultivariateGenerator<Vector, RNGPolicy> {
 public:
  LFMCPPGenerator() = delete;
  LFMCPPGenerator(const LFMCPPGenerator& other);
  LFMCPPGenerator(LFMCPPGenerator&& other) = default;

  LFMCPPGenerator(const std::size_t d, const double rate,
                  const double rate_killing, const double rate_drift,
                  std::unique_ptr<RealUnivariateGenerator<double, RNGPolicy>>&
                      jump_generator);

  virtual ~LFMCPPGenerator() = default;

  LFMCPPGenerator& operator=(const LFMCPPGenerator& other);
  LFMCPPGenerator& operator=(LFMCPPGenerator&& other) = default;

  void operator()(Vector& out) final;

 private:
  std::size_t d_;
  double rate_drift_;
  ExpGenerator<RNGPolicy> bv_generator_;
  ExpGenerator<RNGPolicy> wt_generator_;
  ExpGenerator<RNGPolicy> kt_generator_;
  std::unique_ptr<RealUnivariateGenerator<double, RNGPolicy>> jump_generator_;
};  // ExArnoldGenerator

}  // namespace stats
}  // namespace mo

#include <mo/stats/mo_impl/arnold_generator.ipp>
#include <mo/stats/mo_impl/cuadras_auge_generator.ipp>
#include <mo/stats/mo_impl/esm_generator.ipp>
#include <mo/stats/mo_impl/ex_arnold_generator.ipp>
#include <mo/stats/mo_impl/lfm_cpp_generator.ipp>

#endif  // MO_STATS_MO_HPP
