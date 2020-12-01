#include <iterator>

#include <rmolib/type_traits/is_safe_numeric_cast.hpp>
#include <rmolib/type_traits/iterator.hpp>
#include <testthat.h>

context("rmolib/type_traits/**") {
  using namespace rmolib::type_traits;
  test_that("is_safe_numeric_cast is correct") {
    const auto int_to_double = is_safe_numeric_cast_v<double, int>;
    expect_true(int_to_double);

    constexpr auto unsigned_to_size_t =
        is_safe_numeric_cast_v<std::size_t, unsigned>;
    expect_true(unsigned_to_size_t);

    constexpr auto ptrdifft_to_sizet =
        is_safe_numeric_cast_v<std::size_t, std::ptrdiff_t>;
    expect_true(ptrdifft_to_sizet);

    constexpr auto sizet_to_sizet =
        is_safe_numeric_cast_v<std::size_t, std::size_t>;
    expect_true(sizet_to_sizet);

    constexpr auto float_to_double = is_safe_numeric_cast_v<double, float>;
    expect_true(float_to_double);

    constexpr auto long_to_short = is_safe_numeric_cast_v<short, long>;
    expect_false(long_to_short);

    constexpr auto double_to_float = is_safe_numeric_cast_v<float, double>;
    expect_false(double_to_float);
  }

  test_that("is_*_iterator is correct") {
    const auto input_iterator = is_input_iterator_v<decltype(
        std::declval<std::initializer_list<double>>().begin())>;
    expect_true(input_iterator);

    const auto random_access_iterator = is_random_access_iterator_v<decltype(
        std::declval<std::vector<double>>().begin())>;
    expect_true(random_access_iterator);

    const auto output_iterator =
        is_output_iterator_v<std::back_insert_iterator<std::vector<double>>>;
    expect_true(output_iterator);
  }
}
