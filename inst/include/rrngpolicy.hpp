#ifndef RMO_RRNGGENERATOR_HPP
#define RMO_RRNGGENERATOR_HPP

#include <Rversion.h>
#include <Rmath.h>

#include <mo/stats/generator.hpp>

namespace mo {

namespace stats {

class RRNGPolicy : public RNGPolicy {
public:
  RRNGPolicy() = default;
  RRNGPolicy(const RRNGPolicy& other) = default;
  RRNGPolicy(RRNGPolicy&& other) = default;

  ~RRNGPolicy() {}

  RRNGPolicy& operator=(const RRNGPolicy& other) = default;
  RRNGPolicy& operator=(RRNGPolicy&& other) = default;

  virtual inline double unif_rand() override final;
  virtual inline std::size_t R_unif_index(const std::size_t n) override final;
  virtual inline double exp_rand() override final;
  virtual inline double norm_rand() override final;
};


double RRNGPolicy::unif_rand() {
  return static_cast<double>( ::unif_rand() );
}

std::size_t RRNGPolicy::R_unif_index(const std::size_t n) {
#if defined(R_VERSION) && R_VERSION >= R_Version(3, 3, 4)
  return static_cast<std::size_t>( ::R_unif_index(static_cast<double>(n)) );
#else
    /*
      Sample cannot be reimplemented fully backwards compatible because
      of logic changes in between R 3.3 and R 3.4. However, as long
      as the sample population and the number of samples are smaller
      than `INT_MAX`, this emulation should yield the same results.
     */
  return static_cast<std::size_t>( floor(n * ::unif_rand()) );
#endif
}

double RRNGPolicy::exp_rand() {
  return static_cast<double>( ::exp_rand() );
}

double RRNGPolicy::norm_rand() { // # nocov start
  return static_cast<double>( ::norm_rand() );
} // # nocov end

} // stats

} // mo

#endif // RMO_RRNGGENERATOR_HPP
