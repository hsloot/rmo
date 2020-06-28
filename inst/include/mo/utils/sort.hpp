#ifndef MO_UTILS_SORT_HPP
#define MO_UTILS_SORT_HPP

namespace mo {
namespace utils {

template<typename SortVector, typename IndexVector>
void reverse_sort(SortVector& a, IndexVector& ib);

} // utils
} // mo

#include <mo/utils/implementation/reverse_sort.ipp>

#endif // MO_UTILS_SORT_HPP
