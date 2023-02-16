d <- 10L

test_that("ex_intensities parameter is calculated correctly", {
    bf <- AlphaStableBernsteinFunction(alpha = 0.1033993)

    expect_equal(
        exIntensities(bf, d),
        sapply(1:d, function(i) valueOf(bf, d - i, i, d, i))
    )

    uex_intensities <- uexIntensities(bf, d)
    expect_equal(
        exIntensities(bf, d),
        sapply(
            1:d,
            function(i) {
                multiply_binomial_coefficient(uex_intensities[[i]], d, i)
            }
        )
    )
})

test_that("intensities parameter is calculated correctly", {
    bf <- AlphaStableBernsteinFunction(alpha = 0.07608632)

    tmp <- sapply(1:d, function(i) valueOf(bf, d - i, i))
    intensities <- numeric(2^d - 1)
    for (j in seq_along(intensities)) {
        count <- 0
        for (i in 1:d) {
            count <- count + Rcpp__is_within(i, j)
        }
        intensities[j] <- tmp[count]
    }
    expect_equal(intensities(bf, d), intensities)
})

test_that("ex_qmatrix parameter is calculated correctly", {
    bf <- AlphaStableBernsteinFunction(alpha = 0.6101982)

    ex_qmatrix <- matrix(0, nrow = d + 1, ncol = d + 1)
    ex_qmatrix[1, -1] <- exIntensities(bf, d)
    for (i in 1:d) {
        if (i < d) {
            for (j in (i + 1):d) {
                ex_qmatrix[1 + i, 1 + j] <-
                    (d - j + 1) / (d - i + 1) * ex_qmatrix[i, j] +
                    (j + 1 - i) / (d - i + 1) * ex_qmatrix[i, j + 1]
            }
        }
    }
    diag(ex_qmatrix) <- -apply(ex_qmatrix, 1, sum)
    expect_equal(exQMatrix(bf, d), ex_qmatrix)

    expect_equal(exQMatrix(bf, d), exQMatrix(bf, d + 1)[-1, -1])
})

test_that("sum(ex_intensities) is calculated correctly (base case)", {
    bf <- AlphaStableBernsteinFunction(alpha = 0.8598596)

    expect_equal(
        sum(exIntensities(bf, d)),
        valueOf0(bf, d)
    )
})

test_that("sum(ex_intensities) is calculated correctly (corner case)", {
    bf <- AlphaStableBernsteinFunction(log2(2 - 99.999e-2))
    d <- 125

    expect_equal(
        sum(exIntensities(bf, d)),
        valueOf0(bf, d)
    )
})
