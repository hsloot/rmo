#ifndef MO_STATS_RNGPOLICY_HPP
#define MO_STATS_RNGPOLICY_HPP

#include <cstddef>  // for std::size_t

namespace mo {
namespace stats {

class RNGPolicy {
 public:
  virtual ~RNGPolicy() = default;

  virtual double unif_rand() = 0;
  virtual std::size_t R_unif_index(const std::size_t n) = 0;
  virtual double exp_rand() = 0;
  virtual double norm_rand() = 0;
};

}  // namespace stats
}  // namespace mo

#endif  // MO_STATS_RNGPOLICY_HPP
