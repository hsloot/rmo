#pragma once

#include <algorithm>
#include <cstddef>
#include <iterator>
#include <numeric>
#include <type_traits>
#include <utility>
#include <vector>

#include "rmolib/algorithm/shuffle.hpp"
#include "rmolib/type_traits/iterator.hpp"

namespace rmolib {

namespace algorithm {

//! very hacky implementation to mimic using `base::sample.int` in R to
//! reindex a vector uniform at random.
template <typename _RandomAccessIterator, typename _Engine,
          typename _UniformIntDistribution>
void r_shuffle(_RandomAccessIterator first, _RandomAccessIterator last,
               _Engine& engine, _UniformIntDistribution& dist) {
    using std::iterator_traits;
    using value_type = std::decay_t<
        typename iterator_traits<_RandomAccessIterator>::value_type>;

    static_assert(
        type_traits::is_random_access_iterator_v<_RandomAccessIterator>,
        "r_shuffle<>: _RandomAccessIterator is not a random access iterator");

    std::vector<value_type> cpy{first, last};
    std::vector<std::size_t> perm(cpy.size());
    std::iota(perm.begin(), perm.end(), 0);
    algorithm::shuffle(perm.begin(), perm.end(), engine, dist);
    // for r comparability
    std::reverse(perm.begin(), perm.end());
    for (std::size_t i = 0; i < cpy.size(); ++i) first[i] = cpy[perm[i]];
}

struct r_shuffler {
    template <typename _RandomAccessIterator, typename _Engine,
              typename _UniformIntDistribution>
    void operator()(_RandomAccessIterator first, _RandomAccessIterator last,
                    _Engine&& engine, _UniformIntDistribution&& dist) const {
        return r_shuffle(first, last, std::forward<_Engine>(engine),
                         std::forward<_UniformIntDistribution>(dist));
    }
};

}  // namespace algorithm

}  // namespace rmolib
