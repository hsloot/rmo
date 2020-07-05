#ifndef RMO_RRNGGENERATOR_HPP
#define RMO_RRNGGENERATOR_HPP

#include <Rversion.h>
#include <Rmath.h>

#include <mo/stats/rngpolicy.hpp>

namespace mo {
namespace stats {

class RRNGPolicy : public RNGPolicy {
public:
  RRNGPolicy() = default;
  RRNGPolicy(const RRNGPolicy& other) = default;
  RRNGPolicy(RRNGPolicy&& other) = default;

  virtual ~RRNGPolicy() = default;

  RRNGPolicy& operator=(const RRNGPolicy& other) = default;
  RRNGPolicy& operator=(RRNGPolicy&& other) = default;

  virtual double unif_rand() override final;
  virtual std::size_t R_unif_index(const std::size_t n) override final;
  virtual double exp_rand() override final;
  virtual double norm_rand() override final;
};


inline double RRNGPolicy::unif_rand() {
  return static_cast<double>( ::unif_rand() );
}

inline std::size_t RRNGPolicy::R_unif_index(const std::size_t n) {
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

inline double RRNGPolicy::exp_rand() {
  return static_cast<double>( ::exp_rand() );
}

inline double RRNGPolicy::norm_rand() { // # nocov start
  return static_cast<double>( ::norm_rand() );
} // # nocov end

} // stats
} // mo

#endif // RMO_RRNGGENERATOR_HPP
