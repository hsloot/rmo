context("" RMO_TEST_DIST_NAME_STRING) {
  auto i = 0u;
  for (const auto& param : RMO_TEST_ARG_LIST) {
    test_that("" RMO_TEST_DIST_NAME_STRING
              " can be created from param_type - " +
              std::to_string(i)) {
      RMO_TEST_DIST_NAME dist{param};

      RMO_TEST_CHECK_PARAMS(dist, param)
    }

    test_that("" RMO_TEST_DIST_NAME_STRING
              " can be created from param_type alike structures - " +
              std::to_string(i)) {
      generic_param_type generic_param{param};

      RMO_TEST_DIST_NAME dist{RMO_TEST_DIST_NAME::param_type{generic_param}};

      RMO_TEST_CHECK_PARAMS(dist, param)
    }

    test_that("" RMO_TEST_DIST_NAME_STRING "can be copied and compared - " +
              std::to_string(i)) {
      RMO_TEST_DIST_NAME dist{param};
      RMO_TEST_DIST_NAME dist_copy{dist};

      expect_true(dist == dist_copy);
      expect_false(dist != dist_copy);
    }

    // test of the random number generators should be done in R

    ++i;
  }
}
