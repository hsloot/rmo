#include <testthat.h>

#include "rmolib/bit/bit_fill.hpp"
#include "rmolib/bit/bit_test.hpp"

context("rmolib/bit/**") {
    using namespace rmolib::bit;

    test_that("bit_fill fills bits as expected") {
        expect_true(bit_fill(0u, 0u, false) == 0u);
        expect_true(bit_fill(0u, 4u, false) == 0u);
        expect_true(bit_fill(3u, 5u, false) == 0u);

        expect_true(bit_fill(0u, 0u, true) == 0u);
        expect_true(bit_fill(0u, 4u, true) == 15u);
        expect_true(bit_fill(3u, 5u, true) == 24u);
    }

    test_that("bit_test finds set bits") {
        expect_true(bit_test(4u, 2u));
        expect_true(bit_test(6u, 1u));
        expect_true(bit_test(8u, 3u));

        expect_false(bit_test(2u, 0u));
        expect_false(bit_test(3u, 3u));
        expect_false(bit_test(5u, 1u));
    }
}
