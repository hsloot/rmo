#include <cstddef>
#include <iterator>
#include <utility>
#include <vector>

#include <testthat.h>

#include "rmolib/type_traits/is_safe_numeric_cast.hpp"
#include "rmolib/type_traits/iterator.hpp"

context("rmolib/type_traits/**") {
  using namespace rmolib::type_traits;
  test_that("is_safe_numeric_cast is correct") {
    expect_true((is_safe_numeric_cast_v<double, int>));

    expect_true((is_safe_numeric_cast_v<std::size_t, unsigned>));

    expect_true((is_safe_numeric_cast_v<std::size_t, std::ptrdiff_t>));

    expect_true((is_safe_numeric_cast_v<std::size_t, std::size_t>));

    expect_true((is_safe_numeric_cast_v<double, float>));

    expect_false((is_safe_numeric_cast_v<short, long>));

    expect_false((is_safe_numeric_cast_v<float, double>));
  }

  test_that("is_*_iterator is correct") {
    expect_true((is_input_iterator_v<decltype(
                     std::declval<std::initializer_list<double>>().begin())>));

    expect_true((is_random_access_iterator_v<decltype(
                     std::declval<std::vector<double>>().begin())>));

    expect_true(
        (is_output_iterator_v<std::back_insert_iterator<std::vector<double>>>));
  }
}
