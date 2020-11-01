#include <algorithm>

extern "C" {
#include <R_ext/Utils.h>
}

#include <rmolib/algorithm/r_sort.hpp>
#include <testthat.h>
/*
  TODO
  ====
  - More test cases, including different dimensions and edge cases (e.g.
  first==last)
 */

context("r_sort") {
  // unsorted vectors
  std::vector<std::vector<std::tuple<int, std::size_t>>> unsorted_vectors = {
      {{3, 0}, {2, 1}, {1, 2}, {4, 3}, {5, 4}, {2, 5}},
      {{1, 0}, {2, 1}, {3, 2}, {4, 3}, {5, 4}, {6, 5}},
      {{1, 0}, {1, 1}, {1, 2}, {1, 3}, {1, 4}}};
  // case 1: create max heap
  std::vector<std::vector<std::tuple<int, std::size_t>>> after_make_heap_vectors = {
      {{5, 4}, {4, 3}, {2, 5}, {3, 0}, {2, 1}, {1, 2}},
      {{6, 5}, {5, 4}, {3, 2}, {4, 3}, {2, 1}, {1, 0}},
      {{1, 0}, {1, 1}, {1, 2}, {1, 3}, {1, 4}}};
  // case 2: create max heap and pop
  std::vector<std::vector<std::tuple<int, std::size_t>>> after_pop_heap_vectors = {
      {{4, 3}, {3, 0}, {2, 5}, {1, 2}, {2, 1}, {5, 4}},
      {{5, 4}, {4, 3}, {3, 2}, {1, 0}, {2, 1}, {6, 5}},
      {{1, 4}, {1, 1}, {1, 2}, {1, 3}, {1, 0}}};
  // case 3: create max heap and push
  std::vector<std::tuple<int, std::size_t>> new_values_vectors = {
      {3, 6}, {7, 6}, {1, 5}};
  std::vector<std::vector<std::tuple<int, std::size_t>>> after_push_heap_vectors = {
      {{5, 4}, {4, 3}, {3, 6}, {3, 0}, {2, 1}, {1, 2}, {2, 5}},
      {{7, 6}, {5, 4}, {6, 5}, {4, 3}, {2, 1}, {1, 0}, {3, 2}},
      {{1, 0}, {1, 1}, {1, 2}, {1, 3}, {1, 4}, {1, 5}}};
  // case 4: create max heap and sortsort heap with `sort_heap`
  std::vector<std::vector<std::tuple<int, std::size_t>>> after_sort_heap = {
      {{1, 2}, {2, 5}, {2, 1}, {3, 0}, {4, 3}, {5, 4}},
      {{1, 0}, {2, 1}, {3, 2}, {4, 3}, {5, 4}, {6, 5}},
      {{1, 1}, {1, 2}, {1, 3}, {1, 4}, {1, 0}}};
  // case 5: use Rf_revsort equivalent

  for (unsigned i = 0; i < unsorted_vectors.size(); ++i) {
    auto unsorted_values = unsorted_vectors[i];
    auto make_heap_expected = after_make_heap_vectors[i];
    auto pop_heap_expected = after_pop_heap_vectors[i];
    auto new_value = new_values_vectors[i];
    auto push_heap_expected = after_push_heap_vectors[i];
    auto sort_heap_expected = after_sort_heap[i];
    auto comp = [](auto l, auto u) {
      using std::get;

      return get<0>(l) < get<0>(u);
    };

    test_that("rmolib::algorithm::r_make_heap - " + std::to_string(i)) {
      auto values = unsorted_values;
      rmolib::algorithm::r_make_heap(values.begin(), values.end(), comp);
      expect_true(values == make_heap_expected);
    }

    test_that("rmolib::algorithm::r_pop_heap - " + std::to_string(i)) {
      auto values = unsorted_values;
      rmolib::algorithm::r_make_heap(values.begin(), values.end(), comp);
      rmolib::algorithm::r_pop_heap(values.begin(), values.end(), comp);
      expect_true(values == pop_heap_expected);
    }

    test_that("rmolib::algorithm::r_push_heap - " + std::to_string(i)) {
      auto values = unsorted_values;
      rmolib::algorithm::r_make_heap(values.begin(), values.end(), comp);
      values.push_back(new_value);
      rmolib::algorithm::r_push_heap(values.begin(), values.end(), comp);
      expect_true(values == push_heap_expected);
    }

    test_that("rmolib::algorithm::r_sort_heap - " + std::to_string(i)) {
      auto values = unsorted_values;
      rmolib::algorithm::r_make_heap(values.begin(), values.end(), comp);
      rmolib::algorithm::r_sort_heap(values.begin(), values.end(), comp);
      expect_true(values == sort_heap_expected);
    }

    test_that("Rf_revsort - " + std::to_string(i)) {
      auto unsorted_values = unsorted_vectors[i];
      std::vector<double> a_expected(unsorted_values.size());
      std::vector<int> ib_expected(unsorted_values.size());
      int n = unsorted_values.size();
      std::generate(
          a_expected.begin(), a_expected.end(),
          [&unsorted_values, i = 0]() mutable {
            return static_cast<double>(std::get<0>(unsorted_values[i++]));
          });
      std::generate(
          ib_expected.begin(), ib_expected.end(),
          [&unsorted_values, i = 0]() mutable {
            return static_cast<int>(std::get<1>(unsorted_values[i++]));
          });

      Rf_revsort(a_expected.data(), ib_expected.data(), n);

      auto comp = [](auto l, auto u) {
        return std::get<0>(l) > std::get<0>(u);
      };
      rmolib::algorithm::r_sort(unsorted_values.begin(), unsorted_values.end(),
                                comp);
      std::vector<double> a(unsorted_values.size());
      std::vector<int> ib(unsorted_values.size());
      std::generate(a.begin(), a.end(), [&unsorted_values, i = 0]() mutable {
        return static_cast<double>(std::get<0>(unsorted_values[i++]));
      });
      std::generate(ib.begin(), ib.end(), [&unsorted_values, i = 0]() mutable {
        return static_cast<int>(std::get<1>(unsorted_values[i++]));
      });

      expect_true(a == a_expected);
      expect_true(ib == ib_expected);
    }
  }
}
