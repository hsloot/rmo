#ifndef MO_UTILS_SORT_HPP
#define MO_UTILS_SORT_HPP

#include <cstddef>
#include <vector>

namespace mo {
namespace utils {

template<typename Vector>
std::vector<std::size_t> sort_index(const Vector& x);

template<typename SortVector, typename IndexVector>
void reverse_sort(SortVector& a, IndexVector& ib);

} // utils
} // mo

#include <mo/utils/implementation/sort_index.ipp>
#include <mo/utils/implementation/reverse_sort.ipp>

#endif // MO_UTILS_SORT_HPP
