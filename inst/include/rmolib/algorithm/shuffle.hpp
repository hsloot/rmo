#pragma once

#include <iterator>
#include <limits>
#include <type_traits>

#include "rmolib/math/next_integral_value.hpp"
#include "rmolib/type_traits/is_safe_numeric_cast.hpp"
#include "rmolib/type_traits/iterator.hpp"

namespace rmolib {

namespace algorithm {

template <typename _RandomAccessIterator, typename _Engine,
          typename _UniformIntDistribution>
void shuffle(_RandomAccessIterator first, _RandomAccessIterator last,
             _Engine& engine, _UniformIntDistribution& dist) {
  using std::iterator_traits;
  using size_t = std::make_unsigned_t<
      typename iterator_traits<_RandomAccessIterator>::difference_type>;
  using distr_t = _UniformIntDistribution;

  static_assert(
      type_traits::is_random_access_iterator_v<_RandomAccessIterator>,
      "shuffle<>: _RandomAccessIterator is not a random access iterator");
  static_assert(
      type_traits::is_safe_numeric_cast_v<typename distr_t::result_type,
                                          size_t>,
      "shuffle<>: _UniformIntDistribution::result_type to large");

  auto n = last - first;
  for (auto i = n; i > 1; --i) {
    using std::swap;
    using param_t = typename distr_t::param_type;
    swap(first[i - 1], first[dist(engine, param_t(0, i))]);
  }
}

}  // namespace algorithm

}  // namespace rmolib
