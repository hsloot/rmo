#pragma once

#include <functional>
#include <iterator>
#include <utility>

namespace rmolib {

namespace algorithm {

namespace internal {

// heap parent
template <typename _RandomAccessIterator>
inline auto r_heap_parent(_RandomAccessIterator first,
                          _RandomAccessIterator child) {
    // first == node --> -1 / 2 = 0 --> return first;
    return std::next(first, (std::distance(first, child) - 1) / 2);
}

template <typename _RandomAccessIterator>
inline auto r_last_parent(_RandomAccessIterator first,
                          _RandomAccessIterator last) {
    auto last_child = next(last, -1);
    return r_heap_parent(first, last_child);
};

// heap child
template <typename _RandomAccessIterator, bool left = false>
inline auto r_heap_child(_RandomAccessIterator first,
                         _RandomAccessIterator last,
                         _RandomAccessIterator parent) {
    const auto n = std::distance(first, last);
    const auto k = std::distance(first, parent);
    const auto shift = static_cast<decltype(n)>(left);
    auto child = parent;
    if (2 * k + 1 + shift < n) std::advance(child, k + 1 + shift);
    return child;
}

template <typename _RandomAccessIterator>
inline auto r_heap_left_child(_RandomAccessIterator first,
                              _RandomAccessIterator last,
                              _RandomAccessIterator parent) {
    return r_heap_child<_RandomAccessIterator, false>(first, last, parent);
}

template <typename _RandomAccessIterator>
inline auto r_heap_right_child(_RandomAccessIterator first,
                               _RandomAccessIterator last,
                               _RandomAccessIterator parent) {
    return r_heap_child<_RandomAccessIterator, true>(first, last, parent);
}

// heap sift down
template <typename _RandomAccessIterator, typename _CompareOperator>
inline void r_heap_sift_down(_RandomAccessIterator first,
                             _RandomAccessIterator last,
                             _RandomAccessIterator node,
                             _CompareOperator comp) {
    auto largest = node;
    auto left = r_heap_left_child(first, last, node);
    auto right = r_heap_right_child(first, last, node);
    if (comp(*largest, *left)) largest = left;
    if (comp(*largest, *right)) largest = right;
    if (node != largest) {
        std::swap(*node, *largest);
        r_heap_sift_down(first, last, largest, comp);
    }
}

// heap sift up
template <typename _RandomAccessIterator, typename _CompareOperator>
inline void r_heap_sift_up(_RandomAccessIterator first,
                           _RandomAccessIterator last,
                           _RandomAccessIterator node, _CompareOperator comp) {
    auto parent = r_heap_parent(first, node);
    if (comp(*parent, *node)) {
        std::swap(*node, *parent);
        node = parent;
        r_heap_sift_up(first, last, node, comp);
    }
}

}  // namespace internal

// push heap
template <typename _RandomAccessIterator, typename _CompareOperator>
inline void r_push_heap(_RandomAccessIterator first, _RandomAccessIterator last,
                        _CompareOperator comp) {
    internal::r_heap_sift_up(first, last, next(last, -1), comp);
}

template <typename _RandomAccessIterator, typename _CompareOperator>
inline void r_push_heap(_RandomAccessIterator first,
                        _RandomAccessIterator last) {
    using value_type = typename _RandomAccessIterator::value_type;
    using less = std::less<value_type>;

    r_push_heap(first, last, less{});
}

// pop heap
template <typename _RandomAccessIterator, typename _CompareOperator>
inline void r_pop_heap(_RandomAccessIterator first, _RandomAccessIterator last,
                       _CompareOperator comp) {
    if (std::distance(first, last) > 1) {
        std::advance(last, -1);
        std::swap(*first, *last);
        internal::r_heap_sift_down(first, last, first, comp);
    }
}

template <typename _RandomAccessIterator>
inline void r_pop_heap(_RandomAccessIterator first,
                       _RandomAccessIterator last) {
    using value_type = typename _RandomAccessIterator::value_type;
    using less = std::less<value_type>;

    r_pop_heap(first, last, less{});
}

// make heap
template <typename _RandomAccessIterator, typename _CompareOperator>
inline void r_make_heap(_RandomAccessIterator first, _RandomAccessIterator last,
                        _CompareOperator comp) {
    for (auto parent = internal::r_last_parent(first, last);; --parent) {
        internal::r_heap_sift_down(first, last, parent, comp);
        if (first == parent) break;
    }
}

template <typename _RandomAccessIterator>
inline void r_make_heap(_RandomAccessIterator first,
                        _RandomAccessIterator last) {
    using value_type = typename _RandomAccessIterator::value_type;
    using less = std::less<value_type>;

    r_make_heap(first, last, less{});
}

// sort heap
template <typename _RandomAccessIterator, typename _CompareOperator>
inline void r_sort_heap(_RandomAccessIterator first, _RandomAccessIterator last,
                        _CompareOperator comp) {
    for (auto node = last; first != node; --node) {
        r_pop_heap(first, node, comp);
    }
}

template <typename _RandomAccessIterator>
inline void r_sort_heap(_RandomAccessIterator first,
                        _RandomAccessIterator last) {
    using value_type = typename _RandomAccessIterator::value_type;
    using less = std::less<value_type>;

    r_sort_heap(first, last, less{});
}

// sort
template <typename _RandomAccessIterator, typename _CompareOperator>
inline void r_sort(_RandomAccessIterator first, _RandomAccessIterator last,
                   _CompareOperator comp) {
    /*
    equivalent to sorting algorithm to revsort (but in ascending order) from
    https://github.com/wch/r-source/blob/trunk/src/main/sort.c#334
     */
    r_make_heap(first, last, comp);
    r_sort_heap(first, last, comp);
}

template <typename _RandomAccessIterator>
inline void r_sort(_RandomAccessIterator first, _RandomAccessIterator last) {
    using value_type = typename _RandomAccessIterator::value_type;
    using less = std::less<value_type>;

    r_sort(first, last, less{});
}

}  // namespace algorithm

}  // namespace rmolib
