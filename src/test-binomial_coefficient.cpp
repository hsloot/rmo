#include <functional>
#include <limits>

#include <rmolib/math/binomial_coefficient.hpp>
#include <testthat.h>

context("rmolib/math/**") {
  using namespace rmolib::math;
  test_that("binomial coefficients are calc. correctly") {
    expect_true(binomial_coefficient(10, -1) == 0);
    expect_true(binomial_coefficient(10, 0) == 1);
    expect_true(binomial_coefficient(10, 1) == 10);
    expect_true(binomial_coefficient(10, 2) == 45);
    expect_true(binomial_coefficient(10, 3) == 120);
    expect_true(binomial_coefficient(10, 4) == 210);
    expect_true(binomial_coefficient(10, 5) == 252);
    expect_true(binomial_coefficient(10, 6) == 210);
    expect_true(binomial_coefficient(10, 7) == 120);
    expect_true(binomial_coefficient(10, 8) == 45);
    expect_true(binomial_coefficient(10, 9) == 10);
    expect_true(binomial_coefficient(10, 10) == 1);
    expect_true(binomial_coefficient(10, 12) == 0);
  }

  test_that("inverse binomial coefficients are calc. correctly") {
    expect_true(multiply_binomial_coefficient(
                    1., 9, 0, std::divides<double>{}) == Approx(1.));
    expect_true(multiply_binomial_coefficient(
                    1., 9, 1, std::divides<double>{}) == Approx(1. / 9.));
    expect_true(multiply_binomial_coefficient(
                    1., 9, 2, std::divides<double>{}) == Approx(1. / 36.));
    expect_true(multiply_binomial_coefficient(
                    1., 9, 3, std::divides<double>{}) == Approx(1. / 84.));
    expect_true(multiply_binomial_coefficient(
                    1., 9, 4, std::divides<double>{}) == Approx(1. / 126.));
    expect_true(multiply_binomial_coefficient(
                    1., 9, 5, std::divides<double>{}) == Approx(1. / 126.));
    expect_true(multiply_binomial_coefficient(
                    1., 9, 6, std::divides<double>{}) == Approx(1. / 84.));
    expect_true(multiply_binomial_coefficient(
                    1., 9, 7, std::divides<double>{}) == Approx(1. / 36.));
    expect_true(multiply_binomial_coefficient(
                    1., 9, 8, std::divides<double>{}) == Approx(1. / 9.));
    expect_true(multiply_binomial_coefficient(
                    1., 9, 9, std::divides<double>{}) == Approx(1. / 1.));
  }
}
