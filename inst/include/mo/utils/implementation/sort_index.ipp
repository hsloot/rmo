#ifndef MO_UTILS_IMPLEMENTATION_SORT_INDEX_IPP
#define MO_UTILS_IMPLEMENTATION_SORT_INDEX_IPP

#include <algorithm>
#include <cstddef>
#include <numeric>
#include <vector>

#include <mo/utils/sort.hpp>

namespace mo {
namespace utils {

template <typename Vector>
inline std::vector<std::size_t> sort_index(const Vector& x) {
  std::vector<std::size_t> index(x.size());
  iota(index.begin(), index.end(), 0);
  std::sort(index.begin(), index.end(),
            [&x](size_t i1, size_t i2) { return x[i1] < x[i2]; });
  return index;
}

}  // namespace utils
}  // namespace mo

#endif  // MO_UTILS_IMPLEMENTATION_SORT_INDEX_IPP
