#pragma once

#include <iterator>
#include <string>
#include <type_traits>
#include <utility>
#include <vector>

#include <testthat.h>

// clang-format off
#include "rmolib/random/r_engine.hpp" // must be included before <rmolib/*>
// clang-format on

template <typename _Distribution, typename _GenericParamType>
class tester_distribution {
   public:
    using distribution_type = _Distribution;
    using param_type = typename distribution_type::param_type;
    using generic_param_type = _GenericParamType;

    template <typename _InputIterator>
    tester_distribution(const std::string name, _InputIterator first,
                        _InputIterator last)
        : name_{std::move(name)} {
        __init(first, last);
    }

    tester_distribution(const std::string name,
                        const std::vector<generic_param_type>& wl)
        : tester_distribution{std::move(name), wl.cbegin(), wl.cend()} {}

    tester_distribution(const std::string name,
                        std::initializer_list<generic_param_type> wl)
        : tester_distribution{std::move(name), wl.begin(), wl.end()} {}

    template <typename _Engine>
    void run_tests(_Engine&& engine) {
        for (const auto& [test_num, generic_test_parm] : test_cases_) {
            test_that((name_ + " can be created from param_type - " +
                       std::to_string(test_num))) {
                __param_test(generic_test_parm);
            }
            test_that((name_ + "can be copied and compared - " +
                       std::to_string(test_num))) {
                __copy_test(generic_test_parm);
            }
            test_that((name_ + "can be sampled within bounds - " +
                       std::to_string(test_num))) {
                __sample_test(engine, generic_test_parm);
            }
        }
    }

   private:
    using test_case_t = std::pair<unsigned, generic_param_type>;
    std::string name_{};
    std::vector<test_case_t> test_cases_{};

    class local_rngscope {
       public:
        local_rngscope() { GetRNGstate(); }

        ~local_rngscope() { PutRNGstate(); }
    };

    void __param_test(const generic_param_type& test_parm) const;
    void __copy_test(const generic_param_type& test_parm) const {
        const auto dist = distribution_type{test_parm};
        const auto dist_copy{dist};
        const auto dist_copy_assignment = dist;

        expect_true(dist == dist_copy);
        expect_false(dist != dist_copy);

        expect_true(dist == dist_copy_assignment);
        expect_false(dist != dist_copy_assignment);
    }

    template <typename _Engine>
    void __sample_test(_Engine&& engine, const generic_param_type& test_parm) {
        if constexpr (std::is_same_v<std::decay_t<_Engine>, r_engine>)
            local_rngscope{};  // required to avoid UB

        const auto parm = param_type{test_parm};
        auto default_dist = distribution_type{};
        auto dist = distribution_type{parm};

        const auto is_in_interval = [](const auto x, const auto a,
                                       const auto b) {
            return x >= a && x <= b;
        };

        expect_true(is_in_interval(dist(engine), dist.min(), dist.max()));
        expect_true(
            is_in_interval(default_dist(engine, parm), dist.min(), dist.max()));
    }

    void __init_empty() {
        test_cases_.clear();
        test_cases_.shrink_to_fit();
    }

    template <typename _InputIterator>
    void __init(_InputIterator first, _InputIterator last,
                std::input_iterator_tag) {
        const std::vector<generic_param_type> tmp{first, last};
        __init(tmp.cbegin(), tmp.cend());
    }

    template <typename _ForwardIterator>
    void __init(_ForwardIterator first, _ForwardIterator last,
                std::forward_iterator_tag) {
        using std::distance;
        const auto n = distance(first, last);
        test_cases_.clear();
        test_cases_.reserve(n);
        for (auto [i, it] = std::make_pair(unsigned{0}, first); it != last;
             ++it, ++i)
            test_cases_.emplace_back(std::make_pair(i, *it));
        test_cases_.shrink_to_fit();
    }

    template <typename _InputIterator>
    void __init(_InputIterator first, _InputIterator last) {
        if (first == last) {
            __init_empty();
        } else {
            typename std::iterator_traits<_InputIterator>::iterator_category
                iterator_category;
            __init(first, last, iterator_category);
        }
    }
};
