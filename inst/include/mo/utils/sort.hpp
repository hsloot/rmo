#ifndef MO_UTILS_SORT_HPP
#define MO_UTILS_SORT_HPP

#include <vector>

namespace mo {
namespace utils {

template<typename T, typename S>
void reverse_sort(T& a, S& ib);

} // utils
} // mo

#include <mo/utils/implementation/reverse_sort.ipp>

#endif // MO_UTILS_SORT_HPP
