context("" RMO_TEST_DIST_NAME_STRING) {
  auto i = 0u;
  for (const auto& parm : RMO_TEST_ARG_LIST) {
    test_that("" RMO_TEST_DIST_NAME_STRING
              " can be created from param_type - " +
              std::to_string(i)) {
      RMO_TEST_DIST_NAME dist{parm};

      RMO_TEST_CHECK_PARAMS(dist, parm)
    }

    test_that("" RMO_TEST_DIST_NAME_STRING "can be copied and compared - " +
              std::to_string(i)) {
      RMO_TEST_DIST_NAME dist{parm};
      RMO_TEST_DIST_NAME dist_copy{dist};

      expect_true(dist == dist_copy);
      expect_false(dist != dist_copy);
    }

    // test of the random number generators should be done in R

    ++i;
  }
}
