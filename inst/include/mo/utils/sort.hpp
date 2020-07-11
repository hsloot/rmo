#ifndef MO_UTILS_SORT_HPP
#define MO_UTILS_SORT_HPP

#include <cstddef>
#include <vector>

namespace mo {
namespace utils {

template <typename Vector>
inline std::vector<std::size_t> sort_index(const Vector& x);

template <typename Vector, typename IndexVector>
inline void reverse_sort(Vector& a, IndexVector& ib);

}  // namespace utils
}  // namespace mo

#include <mo/utils/implementation/reverse_sort.ipp>
#include <mo/utils/implementation/sort_index.ipp>

#endif  // MO_UTILS_SORT_HPP
