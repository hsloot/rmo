context("Set functions")

test_that("is_within work as expected in bivariate setting", {
	expect_equal(is_within(1L, 1L), TRUE)
	expect_equal(is_within(1L, 2L), FALSE)
	expect_equal(is_within(2L, 2L), TRUE)
	expect_equal(is_within(1L, 3L), TRUE)
	expect_equal(is_within(2L, 3L), TRUE)
})

test_that("binary mapping is reversible", {
	binary_mapping_identity <- function(n) {
		i <- 0
		j <- 0
		while (j < n) {
			j <- j+is_within(i+1, n)*2^i
			i <- i+1
		}
		j
	}
	expect_equal(binary_mapping_identity(1), 1)
	expect_equal(binary_mapping_identity(5), 5)
	expect_equal(binary_mapping_identity(31), 31)
	expect_equal(binary_mapping_identity(1523), 1523)
	expect_equal(binary_mapping_identity(124352435), 124352435)
})
