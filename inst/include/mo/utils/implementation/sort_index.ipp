#ifndef MO_UTILS_IMPLEMENTATION_SORT_INDEX_IPP
#define MO_UTILS_IMPLEMENTATION_SORT_INDEX_IPP

#include <cstddef>
#include <vector>
#include <numeric>
#include <algorithm>

#include <mo/utils/sort.hpp>

namespace mo {
namespace utils {

template<typename Vector>
std::vector<std::size_t> sort_index(const Vector& x) {
  std::vector<std::size_t> idx(x.size());
  iota(idx.begin(), idx.end(), 0);
  std::sort(idx.begin(), idx.end(),
       [&x](size_t i1, size_t i2) {return x[i1] < x[i2];});
 return idx;
}

} // utils
} // mo

#endif // MO_UTILS_IMPLEMENTATION_SORT_INDEX_IPP
