#pragma once

#include <iterator>
#include <type_traits>

namespace rmolib {

namespace type_traits {

namespace iterator_internal {

template <typename _Iterator>
using iterator_traits = std::iterator_traits<_Iterator>;

template <typename _Iterator, typename _IteratorCategory>
struct has_iterator_category
    : public std::integral_constant<
          bool,
          std::is_base_of_v<_IteratorCategory,
                            typename iterator_traits<_Iterator>::iterator_category>> {};

template <typename _Iterator, typename _IteratorCategory>
constexpr bool has_iterator_category_v =
    has_iterator_category<_Iterator, _IteratorCategory>::value;

}  // namespace iterator_internal

// output iterator
template <typename _Iterator>
struct is_output_iterator
    : public iterator_internal::has_iterator_category<_Iterator,
                                             std::output_iterator_tag> {};
//! true if _Iterator's category derives from std::output_iterator_tag
template <typename _Iterator>
constexpr bool is_output_iterator_v = is_output_iterator<_Iterator>::value;

// input iterator
template <typename _Iterator>
struct is_input_iterator
    : public iterator_internal::has_iterator_category<_Iterator,
                                             std::input_iterator_tag> {};
//! true if _Iterator's category derives from std::input_iterator_tag
template <typename _Iterator>
constexpr bool is_input_iterator_v = is_input_iterator<_Iterator>::value;

// forward iterator
template <typename _Iterator>
struct is_forward_iterator
    : public iterator_internal::has_iterator_category<_Iterator,
                                             std::forward_iterator_tag> {};
//! true if _Iterator's category derives from std::forward_iterator_tag
template <typename _Iterator>
constexpr bool is_forward_iterator_v = is_forward_iterator<_Iterator>::value;

// bidirectional iterator
template <typename _Iterator>
struct is_bidirectional_iterator
    : public iterator_internal::has_iterator_category<_Iterator,
                                             std::bidirectional_iterator_tag> {
};
//! true if _Iterator's category derives from std::bidirectional_iterator_tag
template <typename _Iterator>
constexpr bool is_bidirectional_iterator_v =
    is_bidirectional_iterator<_Iterator>::value;

// random access iterator
template <typename _Iterator>
struct is_random_access_iterator
    : public iterator_internal::has_iterator_category<_Iterator,
                                             std::random_access_iterator_tag> {
};
//! true if _Iterator's category derives from std::random_access_iterator_tag
template <typename _Iterator>
constexpr bool is_random_access_iterator_v =
    is_random_access_iterator<_Iterator>::value;

}  // namespace type_traits

}  // namespace rmolib
